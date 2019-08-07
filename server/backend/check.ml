open Lwt.Infix

let get_prefix conf =
  let server_name = Server_configfile.name conf in
  "opam-health-check-"^server_name

let docker_build ~conf ~cached ~stderr ~img_name dockerfile =
  let cache = if cached then [] else ["--pull";"--no-cache"] in
  let stdin, fd = Lwt_unix.pipe () in
  let stdin = `FD_move stdin in
  Lwt_unix.set_close_on_exec fd;
  let label = get_prefix conf in
  begin
    if not cached then
      Oca_lib.exec ~stdin:`Close ~stdout:stderr ~stderr ["docker";"system";"prune";"-af";"--volumes";("--filter=label="^label)]
    else
      Lwt.return_unit
  end >>= fun () ->
  let proc = Oca_lib.exec ~stdin ~stdout:stderr ~stderr (["docker";"build"]@cache@["--label";label;"-t";img_name;"-"]) in
  Oca_lib.write_line_unix fd (Format.sprintf "%a" Dockerfile.pp dockerfile) >>= fun () ->
  Lwt_unix.close fd >>= fun () ->
  proc

let docker_run ~stdout ~stderr img cmd =
  Oca_lib.exec ~stdin:`Close ~stdout ~stderr ("docker"::"run"::"--rm"::img::cmd)

let docker_run_with_stdin_and_volume ~stdin_content ~stdout ~stderr ~volume:(local_dir, docker_dir) img cmd =
  let stdin, fd = Lwt_unix.pipe () in
  let stdin = `FD_move stdin in
  Lwt_unix.set_close_on_exec fd;
  let proc = Oca_lib.exec ~timeout:48 ~stdin ~stdout ~stderr ("docker"::"run"::"--rm"::"-i"::"-v"::(local_dir^":"^docker_dir)::img::cmd) in
  Lwt_list.iter_s (Oca_lib.write_line_unix fd) stdin_content >>= fun () ->
  Lwt_unix.close fd >>= fun () ->
  proc

let rec read_lines fd =
  Lwt_io.read_line_opt fd >>= function
  | Some line -> read_lines fd >|= List.cons line
  | None -> Lwt.return_nil

let docker_run_to_str ~stderr ~img_name cmd =
  let fd, stdout = Lwt_unix.pipe () in
  let proc = docker_run ~stderr ~stdout img_name cmd in
  Lwt_unix.close stdout >>= fun () ->
  read_lines (Lwt_io.of_fd ~mode:Lwt_io.Input fd) >>= fun pkgs ->
  Lwt_unix.close fd >>= fun () ->
  proc >|= fun () ->
  pkgs

let get_img_name ~conf switch =
  let switch = Intf.Compiler.to_string switch in
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

let get_pkgs ~conf ~stderr switch =
  let img_name = get_img_name ~conf switch in
  Oca_lib.write_line_unix stderr "Getting packages list..." >>= fun () ->
  docker_run_to_str ~stderr ~img_name [] >>= fun pkgs ->
  let rgen = Random.int_range (-1) 1 in
  let pkgs = List.filter begin fun pkg ->
    Oca_lib.is_valid_filename pkg &&
    match Intf.Pkg.name (Intf.Pkg.create ~full_name:pkg ~instances:[] ~maintainers:[] ~revdeps:0) with (* TODO: Remove this horror *)
    | "ocaml" | "ocaml-base-compiler" | "ocaml-variants" -> false
    | _ -> true
  end pkgs in
  let pkgs = List.sort (fun _ _ -> Random.run rgen) pkgs in
  let nelts = string_of_int (List.length pkgs) in
  Oca_lib.write_line_unix stderr ("Package list retrieved. "^nelts^" elements to process.") >|= fun () ->
  pkgs

let is_partial_failure logfile =
  Lwt_io.with_file ~mode:Lwt_io.Input (Fpath.to_string logfile) begin fun ic ->
    let rec lookup () =
      Lwt_io.read_line_opt ic >>= function
      | Some "+- The following actions were aborted" -> Lwt.return_true
      | Some _ -> lookup ()
      | None -> Lwt.return_false
    in
    lookup ()
  end

let run_job ~conf ~pool ~stderr ~switch ~num logdir pkg =
  let img_name = get_img_name ~conf switch in
  Lwt_pool.use pool begin fun () ->
    Oca_lib.write_line_unix stderr ("["^num^"] Checking "^pkg^" on "^Intf.Compiler.to_string switch^"...") >>= fun () ->
    let logfile = Server_workdirs.tmplogfile ~pkg ~switch logdir in
    Lwt_unix.openfile (Fpath.to_string logfile) Unix.[O_WRONLY; O_CREAT; O_TRUNC] 0o640 >>= fun stdout ->
    Lwt.catch begin fun () ->
      Lwt.finalize begin fun () ->
        docker_run ~stdout ~stderr:stdout img_name ["opam";"depext";"-ivy";pkg]
      end begin fun () ->
        Lwt_unix.close stdout
      end >>= fun () ->
      Oca_lib.write_line_unix stderr ("["^num^"] succeeded.") >>= fun () ->
      Lwt_unix.rename (Fpath.to_string logfile) (Fpath.to_string (Server_workdirs.tmpgoodlog ~pkg ~switch logdir))
    end begin function
    | Oca_lib.Process_failure 31 ->
        is_partial_failure logfile >>= begin function
        | true ->
            Oca_lib.write_line_unix stderr ("["^num^"] finished with a partial failure.") >>= fun () ->
            Lwt_unix.rename (Fpath.to_string logfile) (Fpath.to_string (Server_workdirs.tmppartiallog ~pkg ~switch logdir))
        | false ->
            Oca_lib.write_line_unix stderr ("["^num^"] failed.") >>= fun () ->
            Lwt_unix.rename (Fpath.to_string logfile) (Fpath.to_string (Server_workdirs.tmpbadlog ~pkg ~switch logdir))
        end
    | Oca_lib.Process_failure 20 ->
        Oca_lib.write_line_unix stderr ("["^num^"] finished with not available.") >>= fun () ->
        Lwt_unix.rename (Fpath.to_string logfile) (Fpath.to_string (Server_workdirs.tmpnotavailablelog ~pkg ~switch logdir))
    | Oca_lib.Process_failure _ | Oca_lib.Internal_failure ->
        Oca_lib.write_line_unix stderr ("["^num^"] finished with an internal failure.") >>= fun () ->
        Lwt_unix.rename (Fpath.to_string logfile) (Fpath.to_string (Server_workdirs.tmpinternalfailurelog ~pkg ~switch logdir))
    | e ->
        Lwt.fail e
    end
  end

let () =
  Lwt.async_exception_hook := begin fun e ->
    let msg = Printexc.to_string e in
    prerr_endline ("Async exception raised: "^msg);
  end

let get_dockerfile ~conf switch =
  let open Dockerfile in
  from "ocaml/opam2:debian-unstable AS base" @@
  workdir "opam-repository" @@
  run "opam switch 4.07" @@
  run "git pull origin master" @@
  run "opam update" @@
  run "opam depext -yui z3" @@
  workdir "/tmp" @@
  run "git clone git://github.com/kit-ty-kate/opam.git" @@
  workdir "/tmp/opam" @@
  run "git checkout opam-health-check" @@
  run "eval $(opam config env) && ./configure" @@
  run "eval $(opam config env) && make lib-ext" @@
  run "eval $(opam config env) && make" @@
  from "ocaml/opam2:debian-unstable-opam" @@
  copy ~from:"base" ~src:["/tmp/opam/opam"] ~dst:"/usr/bin/opam" () @@
  copy ~from:"base" ~src:["/tmp/opam/opam-installer"] ~dst:"/usr/bin/opam-installer" () @@
  run "sudo apt-get update" @@
  workdir "opam-repository" @@
  run "git pull origin master" @@
  run "opam admin cache" @@
  run "opam init -ya --bare --disable-sandboxing ." @@
  run "opam repository add --dont-select beta git://github.com/ocaml/ocaml-beta-repository.git" @@
  run "opam switch create --repositories=default,beta %s" (Intf.Compiler.to_string switch) @@
  run "echo 'archive-mirrors: [\"file:///home/opam/opam-repository/cache\"]' >> /home/opam/.opam/config" @@
  run "opam install -y opam-depext" @@
  Option.map_or ~default:empty (run "%s") (Server_configfile.extra_command conf) @@
  cmd "%s" (Server_configfile.list_command conf)

let with_stderr ~start_time workdir f =
  Oca_lib.mkdir_p (Server_workdirs.ilogdir workdir) >>= fun () ->
  let logfile = Server_workdirs.new_ilogfile ~start_time workdir in
  Lwt_unix.openfile (Fpath.to_string logfile) Unix.[O_WRONLY; O_CREAT; O_APPEND] 0o640 >>= fun stderr ->
  Lwt.finalize (fun () -> f ~stderr) (fun () -> Lwt_unix.close stderr)

let build_switch ~stderr ~cached conf switch =
  let img_name = get_img_name ~conf switch in
  docker_build ~conf ~stderr ~cached ~img_name (get_dockerfile ~conf switch) >>= fun () ->
  get_pkgs ~conf ~stderr switch >|= fun pkgs ->
  (switch, pkgs)

module Pkg_set = Set.Make (String)

let metadata_script = {|
sudo apt-get -qq install bc &> /dev/null

root=/metadata
revdeps_dir=$root/revdeps
maintainers_dir=$root/maintainers

user=$(stat -c '%u' $root)
group=$(stat -c '%g' $root)
sudo="sudo -u #$user -g #$group"

$sudo mkdir $revdeps_dir $maintainers_dir
prev_pkg=
while read pkg; do
    echo $(opam list -s --recursive --depopts --depends-on "$pkg" | wc -l) - 1 | bc | $sudo tee "$revdeps_dir/$pkg" > /dev/null &
    pkg_name=$(echo "$pkg" | sed -E 's/^([^.]*).*$/\1/')
    if [ "$pkg_name" != "$prev_pkg" ]; then
        opam show -f maintainer: "$pkg_name" | sed -E 's/^"(.*)"$/\1/' | $sudo tee "$maintainers_dir/$pkg_name" > /dev/null
        prev_pkg=$pkg_name
    fi
    [ $(jobs | wc -l) -gt $0 ] && wait
done
exit 0
|}

let get_metadata ~conf ~pool ~stderr switch logdir pkgs =
  let img_name = get_img_name ~conf switch in
  Lwt_pool.use pool begin fun () ->
    Oca_lib.write_line_unix stderr ("Getting all the metadata...") >>= fun () ->
    Lwt_unix.getcwd () >>= fun cwd ->
    let metadatadir = Server_workdirs.tmpmetadatadir logdir in
    let metadatadir = Fpath.to_string Fpath.(v cwd // metadatadir) in
    let stdin_content = Pkg_set.elements pkgs in
    let max_thread = string_of_int (Server_configfile.processes conf) in
    docker_run_with_stdin_and_volume ~stdin_content ~stderr ~stdout:stderr ~volume:(metadatadir, "/metadata") img_name ["bash";"-c";metadata_script;max_thread]
  end

let get_git_hash ~stderr switch conf =
  let img_name = get_img_name ~conf switch in
  Oca_lib.write_line_unix stderr "Getting current git hash..." >>= fun () ->
  docker_run_to_str ~stderr ~img_name ["git";"rev-parse";"HEAD"] >>= function
  | [hash] ->
      Oca_lib.write_line_unix stderr ("Current git hash: "^hash) >|= fun () ->
      hash
  | s ->
      let s = String.unlines s in
      Oca_lib.write_line_unix stderr ("Error: cannot parse git hash. Got:\n"^s) >>= fun () ->
      Lwt.fail_with "Something went wrong. See internal log"

let move_tmpdirs_to_final ~stderr logdir workdir =
  let logdir_path = Server_workdirs.get_logdir_path logdir in
  let tmplogdir = Server_workdirs.tmplogdir logdir in
  let metadatadir = Server_workdirs.metadatadir workdir in
  let tmpmetadatadir = Server_workdirs.tmpmetadatadir logdir in
  Lwt_unix.rename (Fpath.to_string tmplogdir) (Fpath.to_string logdir_path) >>= fun () ->
  (* TODO: replace by Oca_lib.rm_rf *)
  Oca_lib.exec ~stdin:`Close ~stdout:stderr ~stderr ["rm";"-rf";Fpath.to_string metadatadir] >>= fun () ->
  Lwt_unix.rename (Fpath.to_string tmpmetadatadir) (Fpath.to_string metadatadir)

let run_and_get_pkgs ~conf ~pool ~stderr logdir pkgs =
  let len_suffix = "/"^string_of_int (List.fold_left (fun n (_, pkgs) -> n + List.length pkgs) 0 pkgs) in
  List.fold_left begin fun (i, jobs, full_pkgs_set) (switch, pkgs) ->
    List.fold_left begin fun (i, jobs, full_pkgs_set) full_name ->
      let i = succ i in
      let num = string_of_int i^len_suffix in
      let job () = run_job ~conf ~pool ~stderr ~switch ~num logdir full_name in
      (i, (fun () -> job () :: jobs ()), Pkg_set.add full_name full_pkgs_set)
    end (i, jobs, full_pkgs_set) pkgs
  end (0, (fun () -> []), Pkg_set.empty) pkgs

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

let run ~on_finished ~is_retry ~conf cache workdir =
  let switches = Option.get_exn (Server_configfile.ocaml_switches conf) in
  if !run_locked then
    failwith "operation locked";
  run_locked := true;
  Lwt.async begin fun () -> Lwt.finalize begin fun () ->
    let start_time = Unix.time () in
    with_stderr ~start_time workdir begin fun ~stderr ->
      begin match switches with
      | switch::switches ->
          build_switch ~stderr ~cached:is_retry conf switch >>= fun hd_pkgs ->
          get_git_hash ~stderr switch conf >>= fun hash ->
          Oca_server.Cache.get_logdirs cache >>= fun old_logdir ->
          let old_logdir = List.head_opt old_logdir in
          let new_logdir = Server_workdirs.new_logdir ~hash ~start_time workdir in
          Server_workdirs.init_base_jobs ~switches:(switch :: switches) new_logdir >>= fun () ->
          let pool = Lwt_pool.create (Server_configfile.processes conf) (fun () -> Lwt.return_unit) in
          Lwt_list.map_p (build_switch ~stderr ~cached:true conf) switches >>= fun tl_pkgs ->
          let (_, jobs, pkgs) = run_and_get_pkgs ~conf ~pool ~stderr new_logdir (hd_pkgs :: tl_pkgs) in
          let metadata_job = get_metadata ~conf ~pool ~stderr switch new_logdir pkgs in
          Lwt.join (jobs ()) >>= fun () ->
          Lwt.pick [metadata_job; Lwt.return ()] >>= fun () ->
          Oca_lib.write_line_unix stderr "Finishing up..." >>= fun () ->
          move_tmpdirs_to_final ~stderr new_logdir workdir >>= fun () ->
          on_finished workdir;
          trigger_slack_webhooks ~stderr ~old_logdir ~new_logdir conf >>= fun () ->
          let end_time = Unix.time () in
          let time_span = end_time -. start_time in
          Oca_lib.write_line_unix stderr ("Done. Operation took: "^string_of_float time_span^" seconds")
      | [] ->
          Oca_lib.write_line_unix stderr "No switches."
      end
    end
  end (fun () -> run_locked := false; Lwt.return_unit) end;
  Lwt.return_unit
