open Lwt.Infix

let get_prefix conf =
  let server_name = Server_configfile.name conf in
  "opam-health-check-"^server_name

let docker_build ~base_dockerfile ~stdout ~stderr ~img c =
  let home = Sys.getenv "HOME" in
  let cap_file = home^"/ocluster.cap" in (* TODO: fix that *)
  let dockerfile_content =
    let open Dockerfile in
    base_dockerfile @@
    run "%s" c
  in
  Lwt_io.with_temp_file (fun (dockerfile, c) ->
    Lwt_io.write c (Dockerfile.string_of_t dockerfile_content) >>= fun () ->
    Lwt_io.flush c >>= fun () ->
    Oca_lib.exec ~stdin:`Close ~stdout ~stderr ["ocluster-client"; "submit-docker"; cap_file; "--cache-hint"; img; "--pool=linux-x86_64"; dockerfile]
  )

let get_img_name ~conf switch =
  let switch = Intf.Switch.switch switch in
  let switch =
    let rec normalize_docker_tag_name = function
      | ('a'..'z' | 'A'..'Z' | '0'..'9' | '-' | '.') as c::cs -> c::normalize_docker_tag_name cs
      | '_'::'_'::_ -> assert false
      | _::cs -> '_'::'_'::normalize_docker_tag_name cs
      | [] -> []
    in
    String.of_list (normalize_docker_tag_name (String.to_list switch))
  in
  get_prefix conf^"-"^switch

let rec read_lines fd =
  Lwt_io.read_line_opt fd >>= function
  | Some line -> read_lines fd >|= List.cons line
  | None -> Lwt.return_nil

let exec_out ~fexec ~fout =
  let fd, stdout = Lwt_unix.pipe () in
  let proc = fexec ~stdout:(`FD_move stdout) in
  fout (Lwt_io.of_fd ~mode:Lwt_io.Input fd) >>= fun res ->
  Lwt_unix.close fd >>= fun () ->
  proc >|= fun () ->
  res

let docker_build_str ~base_dockerfile ~stderr ~img c =
  exec_out ~fout:read_lines ~fexec:(fun ~stdout ->
    docker_build ~base_dockerfile ~stdout ~stderr ~img ("echo @@@ && "^c^" && echo @@@"))
  >|= fun lines ->
  let rec aux ~is_in acc = function
    | "@@@"::_ when is_in -> List.rev acc
    | "@@@"::xs -> aux ~is_in:true [] xs
    | x::xs when is_in -> aux ~is_in (x :: acc) xs
    | _::xs -> aux ~is_in acc xs
    | [] -> []
  in
  aux ~is_in:false [] lines

let get_pkgs ~base_dockerfile ~conf ~stderr switch =
  let img_name = get_img_name ~conf switch in
  Oca_lib.write_line_unix stderr "Getting packages list..." >>= fun () ->
  docker_build_str ~base_dockerfile ~stderr ~img:img_name (Server_configfile.list_command conf) >>= fun pkgs ->
  let pkgs = List.filter begin fun pkg ->
    Oca_lib.is_valid_filename pkg &&
    match Intf.Pkg.name (Intf.Pkg.create ~full_name:pkg ~instances:[] ~maintainers:[] ~revdeps:0) with (* TODO: Remove this horror *)
    | "ocaml" | "ocaml-base-compiler" | "ocaml-variants" | "ocaml-beta" | "ocaml-config" -> false
    | _ -> true
  end pkgs in
  let nelts = string_of_int (List.length pkgs) in
  Oca_lib.write_line_unix stderr ("Package list retrieved. "^nelts^" elements to process.") >|= fun () ->
  pkgs

let failure_kind logfile =
  Lwt_io.with_file ~mode:Lwt_io.Input (Fpath.to_string logfile) begin fun ic ->
    let rec lookup res =
      Lwt_io.read_line_opt ic >>= function
      | Some "+- The following actions failed" -> lookup `Failure
      | Some "+- The following actions were aborted" -> Lwt.return `Partial
      | Some "[ERROR] Package conflict!" -> lookup `NotAvailable
      | Some "This package failed and has been disabled for CI using the 'x-ci-accept-failures' field." -> lookup `AcceptFailures
      | Some _ -> lookup res
      | None -> Lwt.return res
    in
    lookup `Other
  end

let distribution_used = "debian-unstable"

let with_test pkg = {|
elif [ $res = 0 ]; then
    opam reinstall -vty "|}^pkg^{|"
    res=$?
    if [ $res = 20 ]; then
        res=0
    fi
|}

let with_test ~conf pkg =
  if Server_configfile.with_test conf then
    with_test pkg
  else
    ""

let run_script ~conf pkg = {|
opam install -vy "|}^pkg^{|"
res=$?
if [ $res = 31 ]; then
    if opam show -f x-ci-accept-failures: "|}^pkg^{|" | grep -q '"|}^distribution_used^{|"'; then
        echo "This package failed and has been disabled for CI using the 'x-ci-accept-failures' field."
        exit 69
    fi
|}^with_test ~conf pkg^{|
fi
exit $res
|}

let run_job ~conf ~pool ~stderr ~base_dockerfile ~switch ~num logdir pkg =
  let img_name = get_img_name ~conf switch in
  Lwt_pool.use pool begin fun () ->
    Oca_lib.write_line_unix stderr ("["^num^"] Checking "^pkg^" on "^Intf.Switch.switch switch^"...") >>= fun () ->
    let switch = Intf.Switch.name switch in
    let logfile = Server_workdirs.tmplogfile ~pkg ~switch logdir in
    Lwt_unix.openfile (Fpath.to_string logfile) Unix.[O_WRONLY; O_CREAT; O_TRUNC] 0o640 >>= fun stdout ->
    Lwt.catch begin fun () ->
      Lwt.finalize begin fun () ->
        docker_build ~base_dockerfile ~stdout:(`FD_copy stdout) ~stderr:stdout ~img:img_name (run_script ~conf pkg)
      end begin fun () ->
        Lwt_unix.close stdout
      end >>= fun () ->
      Oca_lib.write_line_unix stderr ("["^num^"] succeeded.") >>= fun () ->
      Lwt_unix.rename (Fpath.to_string logfile) (Fpath.to_string (Server_workdirs.tmpgoodlog ~pkg ~switch logdir))
    end begin function
    | Oca_lib.Process_failure _ -> (* TODO: It might be worth distinguishing between "killed by signal" failures and timeouts, in the future *)
        failure_kind logfile >>= begin function
        | `Partial ->
            Oca_lib.write_line_unix stderr ("["^num^"] finished with a partial failure.") >>= fun () ->
            Lwt_unix.rename (Fpath.to_string logfile) (Fpath.to_string (Server_workdirs.tmppartiallog ~pkg ~switch logdir))
        | `Failure ->
            Oca_lib.write_line_unix stderr ("["^num^"] failed.") >>= fun () ->
            Lwt_unix.rename (Fpath.to_string logfile) (Fpath.to_string (Server_workdirs.tmpbadlog ~pkg ~switch logdir))
        | `NotAvailable ->
            Oca_lib.write_line_unix stderr ("["^num^"] finished with not available.") >>= fun () ->
            Lwt_unix.rename (Fpath.to_string logfile) (Fpath.to_string (Server_workdirs.tmpnotavailablelog ~pkg ~switch logdir))
        | `Other | `AcceptFailures ->
            Oca_lib.write_line_unix stderr ("["^num^"] finished with an internal failure.") >>= fun () ->
            Lwt_unix.rename (Fpath.to_string logfile) (Fpath.to_string (Server_workdirs.tmpinternalfailurelog ~pkg ~switch logdir))
        end
    | e ->
        Lwt.fail e
    end
  end

let () =
  Lwt.async_exception_hook := begin fun e ->
    let msg = Printexc.to_string e in
    prerr_endline ("Async exception raised: "^msg);
  end

let get_dockerfile ~conf ~opam_repo_commit ~opam_alpha_commit switch =
  let open Dockerfile in
  from ("ocurrent/opam:"^distribution_used^" AS base") @@
  run "git -C opam-repository pull origin master && git -C opam-repository checkout %s" (Filename.quote opam_repo_commit) @@
  run "opam update" @@
  run "git clone git://github.com/kit-ty-kate/opam.git /tmp/opam" @@
  run "git -C /tmp/opam checkout opam-health-check3" @@
  run "sudo apt-get install -yy m4" @@
  run "opam pin add -yn /tmp/opam" @@
  run "opam install -y opam-devel opam-0install-cudf" @@
  run {|mv "$(opam var opam-devel:lib)/opam" opam.exe|} @@
  from ("ocurrent/opam:"^distribution_used^"-opam") @@
  copy ~from:"base" ~src:["/home/opam/opam.exe"] ~dst:"/usr/bin/opam" () @@
  run "sudo apt-get update" @@
  run "git -C opam-repository pull origin master && git -C opam-repository checkout %s" (Filename.quote opam_repo_commit) @@
  (if Server_configfile.enable_dune_cache conf then
     run "git pull git://github.com/kit-ty-kate/opam-repository.git opam-health-check"
   else
     empty
  ) @@
  run "rm -rf /home/opam/.opam && opam init -ya --bare --disable-sandboxing ." @@
  env [
    "OPAMPRECISETRACKING","1"; (* NOTE: See https://github.com/ocaml/opam/issues/3997 *)
    "OPAMEXTERNALSOLVER","builtin-0install";
    "OPAMDEPEXTYES","1";
    "OPAMDROPINSTALLEDPACKAGES","1";
  ] @@
  run "opam repository add --dont-select beta git://github.com/ocaml/ocaml-beta-repository.git" @@
  (if Server_configfile.enable_opam_alpha_repository conf
   then
     run "git clone git://github.com/kit-ty-kate/opam-alpha-repository.git && git -C opam-alpha-repository checkout %s" (Filename.quote opam_alpha_commit) @@
     run "opam repository add --dont-select alpha opam-alpha-repository"
   else empty) @@
  run "opam switch create --repositories=%sbeta,default %s"
    (if Server_configfile.enable_opam_alpha_repository conf then "alpha," else "")
    (Intf.Switch.switch switch) @@
  (if OpamVersionCompare.compare (Intf.Switch.switch switch) "4.08" < 0
   then run "opam install -y ocaml-secondary-compiler" (* NOTE: See https://github.com/ocaml/opam-repository/pull/15404
                                                          and https://github.com/ocaml/opam-repository/pull/15642 *)
   else empty) @@
  Option.map_or ~default:empty (run "%s") (Server_configfile.extra_command conf) @@
  (if Server_configfile.enable_dune_cache conf then
     env [
       "DUNE_CACHE","enabled";
       "DUNE_CACHE_TRANSPORT","direct";
       "DUNE_CACHE_DUPLICATION","copy";
     ]
   else
     empty
  ) @@
  run "opam admin cache /home/opam/.cache/opam" @@
  run "echo 'archive-mirrors: [\"/home/opam/.cache/opam\"]' >> /home/opam/.opam/config"

let with_stderr ~start_time workdir f =
  Oca_lib.mkdir_p (Server_workdirs.ilogdir workdir) >>= fun () ->
  let logfile = Server_workdirs.new_ilogfile ~start_time workdir in
  Lwt_unix.openfile (Fpath.to_string logfile) Unix.[O_WRONLY; O_CREAT; O_APPEND] 0o640 >>= fun stderr ->
  Lwt.finalize (fun () -> f ~stderr) (fun () -> Lwt_unix.close stderr)

module Pkg_set = Set.Make (String)

let get_repo ~stderr ~start_time ~repo = (* TODO: Find better than ~start_time to differenciate *)
  let dirname = Filename.basename repo in
  let dirname = Filename.concat (Filename.get_temp_dir_name ()) (dirname^string_of_int (int_of_float start_time)) in
  Oca_lib.exec ~stdin:`Close ~stdout:(`FD_copy stderr) ~stderr
    ["git";"clone";"git://github.com/"^repo^".git";dirname] >>= fun () ->
  exec_out ~fout:read_lines ~fexec:(fun ~stdout ->
    Oca_lib.exec ~stdin:`Close ~stdout ~stderr
      ["git";"-C";dirname;"rev-parse";"origin/master"])
  >|= fun git_hash ->
  List.hd git_hash

let get_metadata ~stderr ~logdir ~start_time ~repo = (* TODO: Find better (see above) *)
  let dirname = Filename.concat (Filename.get_temp_dir_name ()) (repo^string_of_int (int_of_float start_time)) in
  exec_out ~fout:read_lines ~fexec:(fun ~stdout ->
    Oca_lib.exec ~stdin:`Close ~stdout ~stderr
      ["sh";"-c";"cd "^Filename.quote dirname^" && ls -d packages/*/*"])
  >>= fun pkgs ->
  Oca_lib.mkdir_p (Server_workdirs.tmprevdepsdir logdir) >>= fun () ->
  Oca_lib.mkdir_p (Server_workdirs.tmpmaintainersdir logdir) >>= fun () ->
  let rec aux done_pkgnames = function
    | pkgfile::pkgs ->
        let pkg = List.nth (String.split_on_char '/' pkgfile) 2 in
        let revdep_file = Server_workdirs.tmprevdepsfile ~pkg logdir in
        let revdep_file = Fpath.to_string revdep_file in
        Oca_lib.exec ~stdin:`Close ~stdout:(`FD_copy stderr) ~stderr
          ["sh";"-c";"cd "^dirname^" && echo $(opam admin list -s --recursive --depopts --with-test --with-doc --depends-on "^Filename.quote pkg^" | wc -l) - 1 | bc > "^Filename.quote revdep_file]
        >>= fun () ->
        let pkgname = List.nth (String.split_on_char '.' pkg) 0 in
        if Pkg_set.mem pkgname done_pkgnames then
          aux done_pkgnames pkgs
        else
          let maintainers_file = Server_workdirs.tmpmaintainersfile ~pkg:pkgname logdir in
          let maintainers_file = Fpath.to_string maintainers_file in
          Oca_lib.exec ~stdin:`Close ~stdout:(`FD_copy stderr) ~stderr
            ["sh";"-c";"opam show -f maintainer: "^Filename.quote pkgfile^{| | sed -E 's/^\"(.*)\"\$/\1/' > |}^Filename.quote maintainers_file]
          >>= fun () ->
          aux (Pkg_set.add pkgname done_pkgnames) pkgs
    | [] ->
        Lwt.return_unit
  in
  aux Pkg_set.empty pkgs

let move_tmpdirs_to_final ~stderr logdir workdir =
  let logdir_path = Server_workdirs.get_logdir_path logdir in
  let tmplogdir = Server_workdirs.tmplogdir logdir in
  let metadatadir = Server_workdirs.metadatadir workdir in
  let tmpmetadatadir = Server_workdirs.tmpmetadatadir logdir in
  Lwt_unix.rename (Fpath.to_string tmplogdir) (Fpath.to_string logdir_path) >>= fun () ->
  (* TODO: replace by Oca_lib.rm_rf *)
  Oca_lib.exec ~stdin:`Close ~stdout:(`FD_copy stderr) ~stderr ["rm";"-rf";Fpath.to_string metadatadir] >>= fun () ->
  Lwt_unix.rename (Fpath.to_string tmpmetadatadir) (Fpath.to_string metadatadir)

let run_and_get_pkgs ~conf ~pool ~stderr ~base_dockerfile logdir switches pkgs =
  let rgen = Random.int_range (-1) 1 in
  let pkgs = Pkg_set.to_list pkgs in
  let pkgs = List.sort (fun _ _ -> Random.run rgen) pkgs in
  let len_suffix = "/"^string_of_int (List.length pkgs * List.length switches) in
  List.fold_left begin fun (i, jobs) switch ->
    List.fold_left begin fun (i, jobs) full_name ->
      let i = succ i in
      let num = string_of_int i^len_suffix in
      let job = run_job ~conf ~pool ~stderr ~base_dockerfile ~switch ~num logdir full_name in
      (i, job :: jobs)
    end (i, jobs) pkgs
  end (0, []) switches

let trigger_slack_webhooks ~stderr ~old_logdir ~new_logdir conf =
  let body = match old_logdir with
    | Some old_logdir ->
        let old_logdir = Server_workdirs.get_logdir_name old_logdir in
        let new_logdir = Server_workdirs.get_logdir_name new_logdir in
        Printf.sprintf {|{"text":"The latest check is done. Check out http://check.ocamllabs.io/diff/%s..%s to discover which packages are now broken or fixed"}|} old_logdir new_logdir
    | None ->
        {|{"text":"The first check is done. Check out http://check.ocamllabs.io/ to discover which packages are now broken or fixed"}|}
  in
  Server_configfile.slack_webhooks conf |>
  Lwt_list.iter_p begin fun webhook ->
    Oca_lib.write_line_unix stderr ("Triggering Slack webhook "^Uri.to_string webhook) >>= fun () ->
    Cohttp_lwt_unix.Client.post
      ~headers:(Cohttp.Header.of_list ["Content-type", "application/json"])
      ~body:(`String body)
      webhook
    >>= fun (resp, _body) ->
    match Cohttp.Response.status resp with
    | `OK -> Lwt.return_unit
    | _ -> Oca_lib.write_line_unix stderr ("Something when wrong with slack webhook "^Uri.to_string webhook)
  end

let run_locked = ref false

let is_running () = !run_locked

let wait_current_run_to_finish =
  let rec loop () =
    if is_running () then
      Lwt_unix.sleep 1. >>= loop
    else
      Lwt.return_unit
  in
  loop

(* TODO: Reenable the dune cache mode (maybe with the obuilder mode?) *)

let run ~on_finished ~conf cache workdir =
  let switches = Option.get_exn (Server_configfile.ocaml_switches conf) in
  if !run_locked then
    failwith "operation locked";
  run_locked := true;
  Lwt.async begin fun () -> Lwt.finalize begin fun () ->
    let start_time = Unix.time () in
    with_stderr ~start_time workdir begin fun ~stderr ->
      begin match switches with
      | switch::switches ->
          let timer = Oca_lib.timer_start () in
          get_repo ~stderr ~start_time ~repo:"ocaml/opam-repository" >>= fun hash ->
          get_repo ~stderr ~start_time ~repo:"kit-ty-kate/opam-alpha-repository" >>= fun alpha_hash ->
          let base_dockerfile = get_dockerfile ~conf ~opam_repo_commit:hash ~opam_alpha_commit:alpha_hash switch in
          get_pkgs ~base_dockerfile ~conf ~stderr switch >>= fun hd_pkgs ->
          Oca_server.Cache.get_logdirs cache >>= fun old_logdir ->
          let old_logdir = List.head_opt old_logdir in
          let new_logdir = Server_workdirs.new_logdir ~hash ~start_time workdir in
          Server_workdirs.init_base_jobs ~switches:(switch :: switches) new_logdir >>= fun () ->
          let pool = Lwt_pool.create (Server_configfile.processes conf / 3 * 2) (fun () -> Lwt.return_unit) in
          Lwt_list.map_p (get_pkgs ~base_dockerfile ~stderr ~conf) switches >>= fun tl_pkgs ->
          let pkgs = Pkg_set.of_list (List.concat (hd_pkgs :: tl_pkgs)) in
          Oca_lib.timer_log timer stderr "Initialization" >>= fun () ->
          get_metadata ~stderr ~logdir:new_logdir ~start_time ~repo:"opam-repository" >>= fun () ->
          get_metadata ~stderr ~logdir:new_logdir ~start_time ~repo:"opam-alpha-repository" >>= fun () ->
          Oca_lib.timer_log timer stderr "Metadata collection" >>= fun () ->
          let (_, jobs) = run_and_get_pkgs ~conf ~pool ~stderr ~base_dockerfile new_logdir (switch :: switches) pkgs in
          Lwt.join jobs >>= fun () ->
          Oca_lib.write_line_unix stderr "Finishing up..." >>= fun () ->
          move_tmpdirs_to_final ~stderr new_logdir workdir >>= fun () ->
          on_finished workdir;
          trigger_slack_webhooks ~stderr ~old_logdir ~new_logdir conf >>= fun () ->
          Oca_lib.timer_log timer stderr "Operation"
      | [] ->
          Oca_lib.write_line_unix stderr "No switches."
      end
    end
  end (fun () -> run_locked := false; Lwt.return_unit) end;
  Lwt.return_unit
