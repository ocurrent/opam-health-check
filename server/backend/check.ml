open Lwt.Infix

let cache ~conf =
  let opam_cache = Obuilder_spec.Cache.v "opam-archives" ~target:"/home/opam/.opam/download-cache" in
  if Server_configfile.enable_dune_cache conf then
    let dune_cache = Obuilder_spec.Cache.v "dune-cache" ~target:"/home/opam/.config/dune" in
    [opam_cache; dune_cache]
  else
    [opam_cache]

let network = ["host"]

let obuilder_to_string spec =
  Sexplib0.Sexp.to_string_hum (Obuilder_spec.sexp_of_stage spec)

let ocluster_build ~cap ~conf ~base_obuilder ~stdout ~stderr c =
  let obuilder_content =
    let open Obuilder_spec in
    { base_obuilder with
      ops = base_obuilder.ops @ [run ~cache:(cache ~conf) ~network "%s" c] }
  in
  Capnp_rpc_lwt.Sturdy_ref.connect_exn cap >>= fun service ->
  Capnp_rpc_lwt.Capability.with_ref service @@ fun submission_service ->
  let action = Cluster_api.Submission.obuilder_build (obuilder_to_string obuilder_content) in
  Capnp_rpc_lwt.Capability.with_ref (Cluster_api.Submission.submit submission_service ~urgent:false ~pool:"linux-x86_64" ~action ~cache_hint:"") @@ fun ticket ->
  Capnp_rpc_lwt.Capability.with_ref (Cluster_api.Ticket.job ticket) @@ fun job ->
  Capnp_rpc_lwt.Capability.wait_until_settled job >>= fun () ->
  let proc =
    let rec tail job start =
      Cluster_api.Job.log job start >>= function
      | Error (`Capnp e) -> Lwt_io.write stderr (Fmt.str "Error tailing logs: %a" Capnp_rpc.Error.pp e)
      | Ok ("", _) -> Lwt.return_unit
      | Ok (data, next) -> Lwt_io.write stdout data >>= fun () -> tail job next
    in
    tail job 0L >>= fun () ->
    Cluster_api.Job.result job >>= function
    | Ok _ ->
        Lwt.return (Ok ())
    | Error (`Capnp e) ->
        Lwt_io.write stdout (Fmt.str "%a" Capnp_rpc.Error.pp e) >>= fun () ->
        Lwt.return (Error ())
  in
  (* NOTE: any processes shouldn't take more than 2 hours *)
  let timeout =
    let hours = 2 in
    Lwt_unix.sleep (float_of_int (hours * 60 * 60)) >>= fun () ->
    Lwt_io.write_line stdout "+++ Timeout!! (2 hours) +++" >>= fun () ->
    Lwt_io.write_line stderr ("Command '"^c^"' timed-out ("^string_of_int hours^" hours).") >>= fun () ->
    Lwt.return (Error ())
  in
  Lwt.pick [timeout; proc]

let exec_out ~fexec ~fout =
  let stdin, stdout = Lwt_io.pipe () in
  let proc =
    Lwt.finalize
      (fun () -> fexec ~stdout)
      (fun () -> Lwt_io.close stdout)
  in
  fout ~stdin >>= fun res ->
  Lwt_io.close stdin >>= fun () ->
  proc >|= fun r ->
  (r, res)

let ocluster_build_str ~cap ~conf ~base_obuilder ~stderr c =
  let rec aux ~stdin =
    Lwt_io.read_line_opt stdin >>= function
    | Some "@@@" ->
        let rec aux acc =
          Lwt_io.read_line_opt stdin >>= function
          | Some "@@@" -> Lwt.return (List.rev acc)
          | Some x -> aux (x :: acc)
          | None -> Lwt.return_nil (* Something went wrong, ignore. *)
        in
        aux []
    | Some _ -> aux ~stdin
    | None -> Lwt.return_nil
  in
  exec_out ~fout:aux ~fexec:begin fun ~stdout ->
    ocluster_build ~cap ~conf ~base_obuilder ~stdout ~stderr ("echo @@@ && "^c^" && echo @@@")
  end >>= function
  | (Ok (), r) ->
      Lwt.return r
  | (Error (), _) ->
      Lwt_io.write_line stderr ("Failure in ocluster: "^c) >>= fun () ->
      Lwt.fail_with ("Failure in ocluster: "^c)

let failure_kind logfile =
  Lwt_io.with_file ~mode:Lwt_io.Input (Fpath.to_string logfile) begin fun ic ->
    let rec lookup res =
      Lwt_io.read_line_opt ic >>= function
      | Some "+- The following actions failed" -> lookup `Failure
      | Some "+- The following actions were aborted" -> Lwt.return `Partial
      | Some "[ERROR] Package conflict!" -> lookup `NotAvailable
      | Some "This package failed and has been disabled for CI using the 'x-ci-accept-failures' field." -> lookup `AcceptFailures
      | Some "+++ Timeout!! (2 hours) +++" -> Lwt.return `Timeout
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

let run_job ~cap ~conf ~pool ~stderr ~base_obuilder ~switch ~num logdir pkg =
  Lwt_pool.use pool begin fun () ->
    Lwt_io.write_line stderr ("["^num^"] Checking "^pkg^" on "^Intf.Switch.switch switch^"...") >>= fun () ->
    let switch = Intf.Switch.name switch in
    let logfile = Server_workdirs.tmplogfile ~pkg ~switch logdir in
    Lwt_io.with_file ~flags:Unix.[O_WRONLY; O_CREAT; O_TRUNC] ~perm:0o640 ~mode:Lwt_io.Output (Fpath.to_string logfile) begin fun stdout ->
      ocluster_build ~cap ~conf ~base_obuilder ~stdout ~stderr (run_script ~conf pkg)
    end >>= function
    | Ok () ->
        Lwt_io.write_line stderr ("["^num^"] succeeded.") >>= fun () ->
        Lwt_unix.rename (Fpath.to_string logfile) (Fpath.to_string (Server_workdirs.tmpgoodlog ~pkg ~switch logdir))
    | Error () ->
        failure_kind logfile >>= begin function
        | `Partial ->
            Lwt_io.write_line stderr ("["^num^"] finished with a partial failure.") >>= fun () ->
            Lwt_unix.rename (Fpath.to_string logfile) (Fpath.to_string (Server_workdirs.tmppartiallog ~pkg ~switch logdir))
        | `Failure ->
            Lwt_io.write_line stderr ("["^num^"] failed.") >>= fun () ->
            Lwt_unix.rename (Fpath.to_string logfile) (Fpath.to_string (Server_workdirs.tmpbadlog ~pkg ~switch logdir))
        | `NotAvailable ->
            Lwt_io.write_line stderr ("["^num^"] finished with not available.") >>= fun () ->
            Lwt_unix.rename (Fpath.to_string logfile) (Fpath.to_string (Server_workdirs.tmpnotavailablelog ~pkg ~switch logdir))
        | `Other | `AcceptFailures | `Timeout ->
            Lwt_io.write_line stderr ("["^num^"] finished with an internal failure.") >>= fun () ->
            Lwt_unix.rename (Fpath.to_string logfile) (Fpath.to_string (Server_workdirs.tmpinternalfailurelog ~pkg ~switch logdir))
        end
  end

let () =
  Lwt.async_exception_hook := begin fun e ->
    let msg = Printexc.to_string e in
    prerr_endline ("Async exception raised: "^msg);
  end

let get_obuilder ~conf ~opam_repo_commit ~opam_alpha_commit switch =
  let open Obuilder_spec in
  let cache = cache ~conf in
  stage ~from:("ocurrent/opam:"^distribution_used) begin
    [ user ~uid:1000 ~gid:1000;
      workdir "/home/opam";
      run ~network "git clone git://github.com/kit-ty-kate/opam.git /tmp/opam";
      run "git -C /tmp/opam checkout opam-health-check3";
      run ~network "sudo apt-get update";
      run ~network "sudo apt-get install -yy m4";
      run ~network "opam pin add -yn /tmp/opam";
      run ~network "opam install -y opam-devel opam-0install-cudf";
      run "sudo apt-get purge --autoremove -yy m4";
      run {|sudo mv "$(opam var opam-devel:lib)/opam" /usr/bin/opam|};
      run "rm -rf /tmp/opam";
      run ~network "git -C opam-repository pull origin master && git -C opam-repository checkout %s" (Filename.quote opam_repo_commit);
    ] @
    (if Server_configfile.enable_dune_cache conf then
       [run ~network "git -C opam-repository pull git://github.com/kit-ty-kate/opam-repository.git opam-health-check"]
     else
       []
    ) @ [
      env "OPAMPRECISETRACKING" "1"; (* NOTE: See https://github.com/ocaml/opam/issues/3997 *)
      env "OPAMEXTERNALSOLVER" "builtin-0install";
      env "OPAMDEPEXTYES" "1";
      env "OPAMDROPINSTALLEDPACKAGES" "1";
      run "rm -rf /home/opam/.opam && opam init -ya --bare --disable-sandboxing opam-repository";
      run ~network "opam repository add --dont-select beta git://github.com/ocaml/ocaml-beta-repository.git";
    ] @
    (if Server_configfile.enable_opam_alpha_repository conf then
       [ run ~network "git clone git://github.com/kit-ty-kate/opam-alpha-repository.git && git -C opam-alpha-repository checkout %s" (Filename.quote opam_alpha_commit);
         run "opam repository add --dont-select alpha opam-alpha-repository";
       ]
     else
       []
    ) @ [
      run ~cache ~network "opam switch create --repositories=%sbeta,default %s"
        (if Server_configfile.enable_opam_alpha_repository conf then "alpha," else "")
        (Intf.Switch.switch switch);
    ] @
    (if OpamVersionCompare.compare (Intf.Switch.switch switch) "4.08" < 0 then
       [run ~cache ~network "opam install -y ocaml-secondary-compiler"]
       (* NOTE: See https://github.com/ocaml/opam-repository/pull/15404
                and https://github.com/ocaml/opam-repository/pull/15642 *)
     else
       []
    ) @
    (match Server_configfile.extra_command conf with
     | Some c -> [run ~cache ~network "%s" c]
     | None -> []
    ) @
    (if Server_configfile.enable_dune_cache conf then
       [ env "DUNE_CACHE" "enabled";
         env "DUNE_CACHE_TRANSPORT" "direct";
         env "DUNE_CACHE_DUPLICATION" "copy";
       ]
     else
       []
    )
  end

let get_pkgs ~cap ~conf ~stderr ~opam_repo_commit ~opam_alpha_commit switch =
  let base_obuilder = get_obuilder ~conf ~opam_repo_commit ~opam_alpha_commit switch in
  Lwt_io.write_line stderr "Getting packages list..." >>= fun () ->
  ocluster_build_str ~cap ~conf ~base_obuilder ~stderr (Server_configfile.list_command conf) >>= fun pkgs ->
  let pkgs = List.filter begin fun pkg ->
    Oca_lib.is_valid_filename pkg &&
    match Intf.Pkg.name (Intf.Pkg.create ~full_name:pkg ~instances:[] ~maintainers:[] ~revdeps:0) with (* TODO: Remove this horror *)
    | "ocaml" | "ocaml-base-compiler" | "ocaml-variants" | "ocaml-beta" | "ocaml-config" -> false
    | _ -> true
  end pkgs in
  let nelts = string_of_int (List.length pkgs) in
  Lwt_io.write_line stderr ("Package list retrieved. "^nelts^" elements to process.") >|= fun () ->
  pkgs

let with_stderr ~start_time workdir f =
  Oca_lib.mkdir_p (Server_workdirs.ilogdir workdir) >>= fun () ->
  let logfile = Server_workdirs.new_ilogfile ~start_time workdir in
  Lwt_io.with_file ~flags:Unix.[O_WRONLY; O_CREAT; O_APPEND] ~perm:0o640 ~mode:Lwt_io.Output (Fpath.to_string logfile) begin fun stderr ->
    f ~stderr
  end

module Pkg_set = Set.Make (String)

let get_metadata ~cap ~pool ~conf ~base_obuilder ~done_pkgs ~done_pkgnames ~stderr ~logdir ~pkg ~pkgname =
  Lwt_pool.use pool begin fun () ->
    begin
      if Pkg_set.mem pkg done_pkgs then
        Lwt.return_unit
      else
        ocluster_build_str ~cap ~conf ~base_obuilder ~stderr
          ("opam list -s --recursive --depopts --with-test --with-doc --depends-on "^Filename.quote pkg)
        >>= fun revdeps ->
        Lwt_io.with_file ~mode:Lwt_io.output (Fpath.to_string (Server_workdirs.tmprevdepsfile ~pkg logdir)) (fun c ->
          Lwt_io.write c (string_of_int (List.length revdeps))
        )
    end >>= fun () ->
    begin
      if Pkg_set.mem pkgname done_pkgnames then
        Lwt.return_unit
      else
        ocluster_build_str ~cap ~conf ~base_obuilder ~stderr
          ("(opam show -f maintainer: "^Filename.quote pkgname^{| | sed -E 's/^\"(.*)\"\$/\1/')|})
        >>= fun maintainers ->
        Lwt_io.with_file ~mode:Lwt_io.output (Fpath.to_string (Server_workdirs.tmpmaintainersfile ~pkg:pkgname logdir)) (fun c ->
          Lwt_io.write c (String.concat "\n" maintainers)
        )
    end
  end

let get_commit_hash ~user ~repo =
  Github.Monad.run (Github.Repo.get_ref ~user ~repo ~name:"heads/master" ()) >|= fun r ->
  let r = Github.Response.value r in
  r.Github_t.git_ref_obj.Github_t.obj_sha

let move_tmpdirs_to_final logdir workdir =
  let logdir_path = Server_workdirs.get_logdir_path logdir in
  let tmplogdir = Server_workdirs.tmplogdir logdir in
  let metadatadir = Server_workdirs.metadatadir workdir in
  let tmpmetadatadir = Server_workdirs.tmpmetadatadir logdir in
  Lwt_unix.rename (Fpath.to_string tmplogdir) (Fpath.to_string logdir_path) >>= fun () ->
  Oca_lib.rm_rf metadatadir >>= fun () ->
  Lwt_unix.rename (Fpath.to_string tmpmetadatadir) (Fpath.to_string metadatadir)

let run_and_get_pkgs ~cap ~conf ~pool ~stderr ~opam_repo_commit ~opam_alpha_commit logdir switches pkgs =
  let rgen = Random.int_range (-1) 1 in
  let pkgs = Pkg_set.to_list pkgs in
  let pkgs = List.sort (fun _ _ -> Random.run rgen) pkgs in
  let len_suffix = "/"^string_of_int (List.length pkgs * List.length switches) in
  List.fold_left begin fun (i, done_pkgs, done_pkgnames, jobs) full_name ->
    List.fold_left begin fun (i, done_pkgs, done_pkgnames, jobs) switch ->
      let base_obuilder = get_obuilder ~conf ~opam_repo_commit ~opam_alpha_commit switch in
      let i = succ i in
      let num = string_of_int i^len_suffix in
      let job = run_job ~cap ~conf ~pool ~stderr ~base_obuilder ~switch ~num logdir full_name in
      let pkgname = Intf.Pkg.name (Intf.Pkg.create ~full_name ~instances:[] ~maintainers:[] ~revdeps:0) in (* TODO: Remove this horror *)
      let metadata_job = get_metadata ~cap ~pool ~conf ~base_obuilder ~done_pkgs ~done_pkgnames ~stderr ~logdir ~pkg:full_name ~pkgname in
      (i, Pkg_set.add full_name done_pkgs, Pkg_set.add pkgname done_pkgnames, job :: metadata_job :: jobs)
    end (i, done_pkgs, done_pkgnames, jobs) switches
  end (0, Pkg_set.empty, Pkg_set.empty, []) pkgs

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
    Lwt_io.write_line stderr ("Triggering Slack webhook "^Uri.to_string webhook) >>= fun () ->
    Cohttp_lwt_unix.Client.post
      ~headers:(Cohttp.Header.of_list ["Content-type", "application/json"])
      ~body:(`String body)
      webhook
    >>= fun (resp, _body) ->
    match Cohttp.Response.status resp with
    | `OK -> Lwt.return_unit
    | _ -> Lwt_io.write_line stderr ("Something when wrong with slack webhook "^Uri.to_string webhook)
  end

let get_cap ~stderr =
  let home = Sys.getenv "HOME" in
  let cap_file = home^"/ocluster.cap" in (* TODO: fix that *)
  let vat = Capnp_rpc_unix.client_only_vat () in
  match Capnp_rpc_unix.Cap_file.load vat cap_file with
  | Ok sr ->
      Lwt.return sr
  | Error _ ->
      Lwt_io.write_line stderr "cap file couldn't be loaded" >>= fun () ->
      Lwt.fail_with "cap file not found"

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

let run ~on_finished ~conf cache workdir =
  let switches = Option.get_exn (Server_configfile.ocaml_switches conf) in
  if !run_locked then
    failwith "operation locked";
  run_locked := true;
  Lwt.async begin fun () -> Lwt.finalize begin fun () ->
    let start_time = Unix.time () in
    with_stderr ~start_time workdir begin fun ~stderr ->
      begin match switches with
      | _::_ ->
          let timer = Oca_lib.timer_start () in
          get_cap ~stderr >>= fun cap ->
          get_commit_hash ~user:"ocaml" ~repo:"opam-repository" >>= fun opam_repo_commit ->
          get_commit_hash ~user:"kit-ty-kate" ~repo:"opam-alpha-repository" >>= fun opam_alpha_commit ->
          Oca_server.Cache.get_logdirs cache >>= fun old_logdir ->
          let old_logdir = List.head_opt old_logdir in
          let new_logdir = Server_workdirs.new_logdir ~hash:opam_repo_commit ~start_time workdir in
          Server_workdirs.init_base_jobs ~switches new_logdir >>= fun () ->
          let pool = Lwt_pool.create (Server_configfile.processes conf / 3 * 2) (fun () -> Lwt.return_unit) in
          Lwt_list.map_p (get_pkgs ~cap ~stderr ~conf ~opam_repo_commit ~opam_alpha_commit) switches >>= fun pkgs ->
          let pkgs = Pkg_set.of_list (List.concat pkgs) in
          Oca_lib.timer_log timer stderr "Initialization" >>= fun () ->
          let (_, _, _, jobs) = run_and_get_pkgs ~cap ~conf ~pool ~stderr ~opam_repo_commit ~opam_alpha_commit new_logdir switches pkgs in
          Lwt.join jobs >>= fun () ->
          Lwt_io.write_line stderr "Finishing up..." >>= fun () ->
          move_tmpdirs_to_final new_logdir workdir >>= fun () ->
          on_finished workdir;
          trigger_slack_webhooks ~stderr ~old_logdir ~new_logdir conf >>= fun () ->
          Oca_lib.timer_log timer stderr "Operation"
      | [] ->
          Lwt_io.write_line stderr "No switches."
      end
    end
  end (fun () -> run_locked := false; Lwt.return_unit) end;
  Lwt.return_unit
