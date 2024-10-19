let fmt = Printf.sprintf

let cache ~conf =
  let os = Server_configfile.platform_os conf in
  let opam_cache = match os with
    | "freebsd"
    | "linux" -> Some (Obuilder_spec.Cache.v "opam-archives" ~target:"/home/opam/.opam/download-cache")
    | "macos" -> Some (Obuilder_spec.Cache.v "opam-archives" ~target:"/Users/mac1000/.opam/download-cache")
    | "windows" -> Some (Obuilder_spec.Cache.v "opam-archives" ~target:"/Users/opam/AppData/Local/opam/download-cache")
    | os -> failwith ("Opam cache not supported on '" ^ os) (* TODO: Should other platforms simply take the same ocurrent/opam: prefix? *)
  in
  let brew_cache = match os with
    | "macos" -> Some (Obuilder_spec.Cache.v "homebrew" ~target:"/Users/mac1000/Library/Caches/Homebrew")
    | _ -> None
  in
  let dune_cache =
    if Server_configfile.enable_dune_cache conf
    then Some (Obuilder_spec.Cache.v "opam-dune-cache" ~target:"/home/opam/.cache/dune")
    else None
  in
  List.filter_map (fun x -> x) [opam_cache; brew_cache; dune_cache]

let network = ["host"]

let obuilder_to_string spec =
  Sexplib0.Sexp.to_string_mach (Obuilder_spec.sexp_of_t spec)

let ocluster_build ~cap ~conf ~base_obuilder ~stdout ~stderr c =
  let obuilder_content =
    let {Obuilder_spec.child_builds; from; ops} = base_obuilder in
    Obuilder_spec.stage
      ~child_builds
      ~from
      (ops @ [Obuilder_spec.run ~cache:(cache ~conf) ~network "%s" c])
  in
  let obuilder_content = obuilder_to_string obuilder_content in
  let%lwt service = Capnp_rpc_lwt.Sturdy_ref.connect_exn cap in
  Capnp_rpc_lwt.Capability.with_ref service @@ fun submission_service ->
  let action = Cluster_api.Submission.obuilder_build obuilder_content in
  let cache_hint = "opam-health-check-"^Digest.to_hex (Digest.string obuilder_content) in
  let pool = Server_configfile.platform_pool conf in
  Capnp_rpc_lwt.Capability.with_ref (Cluster_api.Submission.submit submission_service ~urgent:false ~pool ~action ~cache_hint) @@ fun ticket ->
  Capnp_rpc_lwt.Capability.with_ref (Cluster_api.Ticket.job ticket) @@ fun job ->
  match%lwt Capnp_rpc_lwt.Capability.await_settled job with
  | Ok () ->
      let proc =
        let rec tail job start =
          match%lwt Cluster_api.Job.log job start with
          | Error (`Capnp e) -> Lwt_io.write stderr (Fmt.str "Error tailing logs: %a" Capnp_rpc.Error.pp e)
          | Ok ("", _) -> Lwt.return_unit
          | Ok (data, next) ->
              let%lwt () = Lwt_io.write stdout data in
              tail job next
        in
        let%lwt () = tail job 0L in
        match%lwt Cluster_api.Job.result job with
        | Ok _ ->
            Lwt.return (Ok ())
        | Error (`Capnp e) ->
            let%lwt () = Lwt_io.write stdout (Fmt.str "%a" Capnp_rpc.Error.pp e) in
            Lwt.return (Error ())
      in
      let timeout =
        let hours = Server_configfile.job_timeout conf in
        let%lwt () = Lwt_unix.sleep (hours *. 60.0 *. 60.0) in
        let cancel =
          let%lwt cancel_result = Cluster_api.Job.cancel job in
          let%lwt () = Lwt_io.write_line stdout ("+++ Timeout!! ("^string_of_float hours^" hours) +++") in
          match cancel_result with
          | Ok () ->
              Lwt_io.write_line stdout "+++ Job cancelled +++"
          | Error (`Capnp err) ->
              Lwt_io.write_line stdout (Fmt.str "+++ Could not cancel job: %a +++" Capnp_rpc.Error.pp err)
        in
        let timeout =
          let minute = 1 in
          let%lwt () = Lwt_unix.sleep (float_of_int (minute * 60)) in
          Lwt_io.write_line stdout "+++ Cancellation failed +++"
        in
        let%lwt () = Lwt.pick [cancel; timeout] in
        let%lwt () = Lwt_io.write_line stderr ("Command '"^c^"' timed-out ("^string_of_float hours^" hours).") in
        Lwt.return (Error ())
      in
      Lwt.pick [timeout; proc]
  | Error {Capnp_rpc.Exception.reason; _} ->
      let%lwt () = Lwt_io.write_line stderr ("capnp-rpc failed to settle: "^reason) in
      Lwt.return (Error ())

let exec_out ~fexec ~fout =
  let stdin, stdout = Lwt_io.pipe () in
  let proc = (fexec ~stdout) [%lwt.finally Lwt_io.close stdout] in
  let%lwt res = fout ~stdin in
  let%lwt () = Lwt_io.close stdin in
  let%lwt r = proc in
  Lwt.return (r, res)

let ocluster_build_str ~important ~debug ~cap ~conf ~base_obuilder ~stderr ~default c =
  let rec aux ~stdin =
    let%lwt line = Lwt_io.read_line_opt stdin in
    match line with
    | Some "@@@OUTPUT" ->
        let rec aux acc =
          match%lwt Lwt_io.read_line_opt stdin with
          | Some "@@@OUTPUT" -> Lwt.return (List.rev acc)
          | Some x -> aux (x :: acc)
          | None when important -> Lwt.fail (Failure "Error: Closing @@@OUTPUT could not be detected")
          | None ->
              let%lwt () = Lwt_io.write_line stderr "Error: Closing @@@OUTPUT could not be detected" in
              Lwt.return_nil
        in
        aux []
    | Some line ->
        let%lwt () = (if debug then Lwt_io.write_line stderr line else Lwt.return_unit) in
        aux ~stdin
    | None -> Lwt.return_nil
  in
  match%lwt
    exec_out ~fout:aux ~fexec:(fun ~stdout ->
      ocluster_build ~cap ~conf ~base_obuilder ~stdout ~stderr ("echo @@@OUTPUT && "^c^" && echo @@@OUTPUT")
    )
  with
  | (Ok (), r) ->
      Lwt.return r
  | (Error (), _) ->
      match default with
      | None -> Lwt.fail (Failure ("Failure in ocluster: "^c)) (* TODO: Replace this with "send message to debug slack webhook" *)
      | Some v -> Lwt.return v

let failure_kind conf ~pkg logfile =
  let pkgname, pkgversion = match Astring.String.cut ~sep:"." pkg with
    | Some x -> x
    | None -> Fmt.failwith "Package %S could not be parsed" pkg
  in
  let timeout = Server_configfile.job_timeout conf in
  Lwt_io.with_file ~mode:Lwt_io.Input (Fpath.to_string logfile) begin fun ic ->
    let rec lookup res =
      match%lwt Lwt_io.read_line_opt ic with
      | Some "+- The following actions failed" -> lookup `Failure
      | Some "+- The following actions were aborted" -> Lwt.return `Partial
      | Some line when String.equal ("[ERROR] No package named "^pkgname^" found.") line ||
                       String.equal ("[ERROR] Package "^pkgname^" has no version "^pkgversion^".") line ->
          lookup `NotAvailable
      | Some "[ERROR] Package conflict!" -> lookup `NotAvailable
      | Some "This package failed and has been disabled for CI using the 'x-ci-accept-failures' field." -> lookup `AcceptFailures
      | Some line when String.equal ("+++ Timeout!! ("^string_of_float timeout^" hours) +++") line -> Lwt.return `Timeout
      | Some line when String.prefix ~pre:"#=== ERROR while fetching sources for " line -> Lwt.return `Other
      | Some _ -> lookup res
      | None -> Lwt.return res
    in
    lookup `Other
  end

let with_test pkg = {|
if [ $res = 0 ]; then
    opam remove -y "|}^pkg^{|"
    opam install -vty "|}^pkg^{|"
    res=$?
    if [ $res = 20 ]; then
        res=0
    fi
fi
|}

let with_test ~conf pkg =
  if Server_configfile.with_test conf then
    with_test pkg
  else
    ""

let with_lower_bound pkg = {|
if [ $res = 0 ]; then
    opam remove -y "|}^pkg^{|"
    env OPAMCRITERIA="+removed,+count[version-lag,solution]" opam install -vy "|}^pkg^{|"
    res=$?
fi
|}

let with_lower_bound ~conf pkg =
  if Server_configfile.with_lower_bound conf then
    with_lower_bound pkg
  else
    ""

let run_script ~conf pkg = {|
opam remove -y "|}^pkg^{|"
opam install -vy "|}^pkg^{|"
res=$?
if [ $res = 31 ]; then
    if opam show -f x-ci-accept-failures: "|}^pkg^{|" | grep -q '"|}^Server_configfile.platform_distribution conf^{|"'; then
        echo "This package failed and has been disabled for CI using the 'x-ci-accept-failures' field."
        exit 69
    fi
fi
|}^with_test ~conf pkg^{|
|}^with_lower_bound ~conf pkg^{|
exit $res
|}

let run_job ~cap ~conf ~pool ~stderr ~base_obuilder ~switch ~num logdir pkg =
  Lwt_pool.use pool begin fun () ->
    let%lwt () = Lwt_io.write_line stderr ("["^num^"] Checking "^pkg^" on "^Intf.Switch.switch switch^"…") in
    let switch = Intf.Switch.name switch in
    let logfile = Server_workdirs.tmplogfile ~pkg ~switch logdir in
    match%lwt
      Lwt_io.with_file ~flags:Unix.[O_WRONLY; O_CREAT; O_TRUNC] ~perm:0o640 ~mode:Lwt_io.Output (Fpath.to_string logfile) (fun stdout ->
        ocluster_build ~cap ~conf ~base_obuilder ~stdout ~stderr (run_script ~conf pkg)
      )
    with
    | Ok () ->
        Prometheus.Counter.inc_one Metrics.jobs_ok;
        let%lwt () = Lwt_io.write_line stderr ("["^num^"] succeeded.") in
        Lwt_unix.rename (Fpath.to_string logfile) (Fpath.to_string (Server_workdirs.tmpgoodlog ~pkg ~switch logdir))
    | Error () ->
        Prometheus.Counter.inc_one Metrics.jobs_error;
        begin match%lwt failure_kind conf ~pkg logfile with
        | `Partial ->
            let%lwt () = Lwt_io.write_line stderr ("["^num^"] finished with a partial failure.") in
            Lwt_unix.rename (Fpath.to_string logfile) (Fpath.to_string (Server_workdirs.tmppartiallog ~pkg ~switch logdir))
        | `Failure ->
            let%lwt () = Lwt_io.write_line stderr ("["^num^"] failed.") in
            Lwt_unix.rename (Fpath.to_string logfile) (Fpath.to_string (Server_workdirs.tmpbadlog ~pkg ~switch logdir))
        | `NotAvailable ->
            let%lwt () = Lwt_io.write_line stderr ("["^num^"] finished with not available.") in
            Lwt_unix.rename (Fpath.to_string logfile) (Fpath.to_string (Server_workdirs.tmpnotavailablelog ~pkg ~switch logdir))
        | `Other | `AcceptFailures | `Timeout ->
            let%lwt () = Lwt_io.write_line stderr ("["^num^"] finished with an internal failure.") in
            Lwt_unix.rename (Fpath.to_string logfile) (Fpath.to_string (Server_workdirs.tmpinternalfailurelog ~pkg ~switch logdir))
        end
  end

let () =
  Lwt.async_exception_hook := begin fun e ->
    prerr_endline ("Async exception raised: "^Printexc.to_string e);
    prerr_string (Printexc.get_backtrace ());
    flush stderr;
  end

let get_obuilder ~conf ~opam_repo ~opam_repo_commit ~extra_repos switch =
  let extra_repos =
    let switch = Intf.Switch.name switch in
    List.filter (fun (repo, _) ->
      match Intf.Repository.for_switches repo with
      | None -> true
      | Some for_switches -> List.exists (Intf.Compiler.equal switch) for_switches
    ) extra_repos
  in
  let open Obuilder_spec in
  let cache = cache ~conf in
  let os = Server_configfile.platform_os conf in
  let distribution = Server_configfile.platform_distribution conf in
  let from = match os with
    | "linux" -> "ocaml/opam:"^distribution (* typically this is 'debian-unstable' which is 5.0.0 *)
    | "freebsd" -> distribution
    | "macos" -> "macos-"^distribution
    | "windows" -> distribution
    | os -> failwith ("OS '"^os^"' not supported") (* TODO: Should other platforms simply take the same ocurrent/opam: prefix? *)
  in
  let ln_opam = match os with
    | "linux" -> "sudo ln -f /usr/bin/opam-dev /usr/bin/opam"
    | "freebsd" -> "sudo ln -f /usr/local/bin/opam-dev /usr/local/bin/opam"
    | "macos" -> "ln -f ~/local/bin/opam-dev ~/local/bin/opam"
    | "windows" -> "ln -f /usr/bin/opam-dev.exe /usr/bin/opam.exe"
    | os -> failwith ("OS '"^os^"' not supported")
  in
  let opam_init_options = match os with
    | "linux" -> " --config ~/.opamrc-sandbox"
    | "freebsd"
    | "windows"
    | "macos" -> ""
    | os -> failwith ("OS '"^os^"' not supported")
  in
  stage ~from begin
    [ user_unix ~uid:1000 ~gid:1000;
      env "OPAMPRECISETRACKING" "1"; (* NOTE: See https://github.com/ocaml/opam/issues/3997 *)
      env "OPAMUTF8" "never"; (* Disable UTF-8 characters so that output stay consistant accross platforms *)
      env "OPAMEXTERNALSOLVER" "builtin-0install";
      env "OPAMCRITERIA" "+removed";
      env "CI" "true"; env "OPAM_HEALTH_CHECK_CI" "true"; (* Advertise CI for test frameworks *)
      run "%s" ln_opam;
      run ~network "rm -rf ~/opam-repository && git clone -q '%s' ~/opam-repository && git -C ~/opam-repository checkout -q %s" (Intf.Github.url opam_repo) opam_repo_commit;
      run "rm -rf ~/.opam && opam init -ya --bare%s ~/opam-repository" opam_init_options;
    ] @
    List.flatten (
      List.map (fun (repo, hash) ->
        let name = Filename.quote (Intf.Repository.name repo) in
        let url = Intf.Github.url (Intf.Repository.github repo) in
        [ run ~network "git clone -q '%s' ~/%s && git -C ~/%s checkout -q %s" url name name hash;
          run "opam repository add --dont-select %s ~/%s" name name;
        ]
      ) extra_repos
    ) @ [
      run ~cache ~network "opam switch create --repositories=%sdefault '%s' '%s'"
        (List.fold_left (fun acc (repo, _) -> Intf.Repository.name repo^","^acc) "" extra_repos)
        (Intf.Compiler.to_string (Intf.Switch.name switch))
        (Intf.Switch.switch switch);
      run ~network "opam update --depexts";
    ] @
    (* TODO: Should this be removed now that it is part of the base docker images? What about macOS? *)
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
       [ run ~cache ~network "opam pin add -k version dune $(opam show -f version dune)";
         env "DUNE_CACHE" "enabled";
         env "DUNE_CACHE_TRANSPORT" "direct";
         env "DUNE_CACHE_DUPLICATION" "copy";
       ]
     else
       []
    )
  end

let get_pkgs ~debug ~cap ~conf ~stderr (switch, base_obuilder) =
  let switch = Intf.Compiler.to_string (Intf.Switch.name switch) in
  let%lwt () = Lwt_io.write_line stderr ("Getting packages list for "^switch^"… (this may take an hour or two)") in
  let%lwt pkgs = ocluster_build_str ~important:true ~debug ~cap ~conf ~base_obuilder ~stderr ~default:None (Server_configfile.list_command conf) in
  let pkgs = List.filter begin fun pkg ->
    Oca_lib.is_valid_filename pkg &&
    match Intf.Pkg.name (Intf.Pkg.create ~full_name:pkg ~instances:[] ~opam:OpamFile.OPAM.empty ~revdeps:0) with (* TODO: Remove this horror *)
    | "ocaml" | "ocaml-base-compiler" | "ocaml-variants" | "ocaml-beta" | "ocaml-config" -> false
    | "ocaml-option-32bit"
    | "ocaml-option-afl"
    | "ocaml-option-bytecode-only"
    | "ocaml-option-default-unsafe-string"
    | "ocaml-option-flambda"
    | "ocaml-option-fp"
    | "ocaml-option-musl"
    | "ocaml-option-nnp"
    | "ocaml-option-nnpchecker"
    | "ocaml-option-no-flat-float-array"
    | "ocaml-option-spacetime"
    | "ocaml-option-static"
    | "ocaml-options-only-afl"
    | "ocaml-options-only-flambda"
    | "ocaml-options-only-flambda-fp"
    | "ocaml-options-only-fp"
    | "ocaml-options-only-nnp"
    | "ocaml-options-only-nnpchecker"
    | "ocaml-options-only-no-flat-float-array"
    | "ocaml-options-vanilla" -> false
    | _ -> true
  end pkgs in
  let nelts = string_of_int (List.length pkgs) in
  let%lwt () = Lwt_io.write_line stderr ("Package list for "^switch^" retrieved. "^nelts^" elements to process.") in
  Lwt.return pkgs

let with_stderr ~start_time workdir f =
  let%lwt () = Oca_lib.mkdir_p (Server_workdirs.ilogdir workdir) in
  let logfile = Server_workdirs.new_ilogfile ~start_time workdir in
  Lwt_io.with_file ~flags:Unix.[O_WRONLY; O_CREAT; O_APPEND] ~perm:0o640 ~mode:Lwt_io.Output (Fpath.to_string logfile) begin fun stderr ->
    f ~stderr
  end

module Pkg_set = Set.Make (String)

let revdeps_script pkg =
  let pkg = Filename.quote pkg in
  {|opam list --color=never -s --recursive --depopts --depends-on |}^pkg^{| && \
    opam list --color=never -s --with-test --with-doc --depopts --depends-on |}^pkg

let get_metadata ~debug ~jobs ~cap ~conf ~pool ~stderr logdir (_, base_obuilder) pkgs =
  let get_revdeps ~base_obuilder ~pkgname ~pkg ~logdir =
    let%lwt revdeps = ocluster_build_str ~important:false ~debug ~cap ~conf ~base_obuilder ~stderr ~default:(Some []) (revdeps_script pkg) in
    let module Set = Set.Make(String) in
    let revdeps = Set.of_list revdeps in
    let revdeps = Set.remove pkgname revdeps in (* https://github.com/ocaml/opam/issues/4446 *)
    Lwt_io.with_file ~mode:Lwt_io.output (Fpath.to_string (Server_workdirs.tmprevdepsfile ~pkg logdir)) (fun c ->
      Lwt_io.write c (string_of_int (Set.cardinal revdeps))
    )
  in
  let get_latest_metadata ~base_obuilder ~pkgname ~logdir = (* TODO: Get this locally by merging all the repository and parsing the opam files using opam-core *)
    let%lwt opam =
      ocluster_build_str ~important:false ~debug ~cap ~conf ~base_obuilder ~stderr ~default:(Some [])
        ("opam show --raw "^Filename.quote pkgname)
    in
    Lwt_io.with_file ~mode:Lwt_io.output (Fpath.to_string (Server_workdirs.tmpopamfile ~pkg:pkgname logdir)) (fun c ->
      Lwt_io.write c (String.concat "\n" opam)
    )
  in
  Pkg_set.fold begin fun full_name (pkgs_set, jobs) ->
    let pkgname = Intf.Pkg.name (Intf.Pkg.create ~full_name ~instances:[] ~opam:OpamFile.OPAM.empty ~revdeps:0) in (* TODO: Remove this horror *)
    let job =
      Lwt_pool.use pool begin fun () ->
        let%lwt () = Lwt_io.write_line stderr ("Getting metadata for "^full_name) in
        let%lwt () = get_revdeps ~base_obuilder ~pkgname ~pkg:full_name ~logdir in
        if Pkg_set.mem pkgname pkgs_set then Lwt.return_unit else get_latest_metadata ~base_obuilder ~pkgname ~logdir
      end
    in
    (Pkg_set.add pkgname pkgs_set, job :: jobs)
  end pkgs (Pkg_set.empty, jobs)

let get_commit_hash github =
  let user = Intf.Github.user github in
  let repo = Intf.Github.repo github in
  let branch = Intf.Github.branch github in
  let%lwt r =
    Github.Monad.run begin
      let ( >>= ) = Github.Monad.( >>= ) in
      Github.Repo.info ~user ~repo () >>= fun info ->
      let branch = match branch with
        | None -> info#value.Github_t.repository_default_branch
        | Some _ -> branch
      in
      let branch = Option.value ~default:"master" branch in
      Github.Repo.get_ref ~user ~repo ~name:("heads/"^branch) ()
    end
  in
  let r = Github.Response.value r in
  Lwt.return (r.Github_t.git_ref_obj.Github_t.obj_sha)

let get_commit_hash_default conf =
  let github = Server_configfile.default_repository conf in
  let%lwt hash = get_commit_hash github in
  Lwt.return (github, hash)

let get_commit_hash_extra_repos conf =
  Lwt_list.map_s begin fun repository ->
    let github = Intf.Repository.github repository in
    let%lwt hash = get_commit_hash github in
    Lwt.return (repository, hash)
  end (Server_configfile.extra_repositories conf)

let move_tmpdirs_to_final ~switches logdir workdir =
  let metadatadir = Server_workdirs.metadatadir workdir in
  let tmpmetadatadir = Server_workdirs.tmpmetadatadir logdir in
  let tmpdir = Server_workdirs.tmpdir logdir in
  let switches = List.map Intf.Switch.name switches in
  let%lwt () = Server_workdirs.logdir_move ~switches logdir in
  let%lwt () = Oca_lib.rm_rf metadatadir in
  let%lwt () = Lwt_unix.rename (Fpath.to_string tmpmetadatadir) (Fpath.to_string metadatadir) in
  Oca_lib.rm_rf tmpdir

let run_jobs ~cap ~conf ~pool ~stderr logdir switches pkgs =
  let len = Pkg_set.cardinal pkgs * List.length switches in
  Prometheus.Gauge.set Metrics.jobs_total (float_of_int len);
  let len_suffix = "/"^string_of_int len in
  Pkg_set.fold begin fun full_name (i, jobs) ->
    List.fold_left begin fun (i, jobs) (switch, base_obuilder) ->
      let i = succ i in
      let num = string_of_int i^len_suffix in
      let job = run_job ~cap ~conf ~pool ~stderr ~base_obuilder ~switch ~num logdir full_name in
      (i, job :: jobs)
    end (i, jobs) switches
  end pkgs (0, [])

let trigger_slack_webhooks ~stderr ~old_logdir ~new_logdir conf =
  let public_url = Server_configfile.public_url conf in
  let body = match old_logdir with
    | Some old_logdir ->
        let old_logdir = Server_workdirs.get_logdir_name old_logdir in
        let new_logdir = Server_workdirs.get_logdir_name new_logdir in
        Printf.sprintf {|{"username": "opam-health-check", "text":"The latest check is done. Check out %s/diff/%s..%s to discover which packages are now broken or fixed"}|} public_url old_logdir new_logdir
    | None ->
        Printf.sprintf {|{"text":"The first check is done. Check out %s to discover which packages are now broken or fixed"}|} public_url
  in
  Server_configfile.slack_webhooks conf |>
  Lwt_list.iter_s begin fun webhook ->
    let%lwt () = Lwt_io.write_line stderr ("Triggering Slack webhook "^Uri.to_string webhook) in
    match%lwt
      Http_lwt_client.request
        ~config:(`HTTP_1_1 Httpaf.Config.default) (* TODO: Remove this when https://github.com/roburio/http-lwt-client/issues/7 is fixed *)
        ~meth:`POST
        ~headers:[("Content-type", "application/json")]
        ~body
        (Uri.to_string webhook)
        (fun _resp acc x -> Lwt.return (acc ^ x)) ""
    with
    | Ok ({Http_lwt_client.status = `OK; _}, _body) -> Lwt.return_unit
    | Ok (resp, body) ->
        let resp = Format.sprintf "%a" Http_lwt_client.pp_response resp in
        Lwt_io.write_line stderr (fmt "Webhook returned failure: %s\nBody: %s" resp body)
    | Error (`Msg msg) -> Lwt_io.write_line stderr ("Webhook failed with: "^msg)
  end

let get_cap ~stderr ~cap_file =
  let vat = Capnp_rpc_unix.client_only_vat () in
  match Capnp_rpc_unix.Cap_file.load vat cap_file with
  | Ok sr ->
      Lwt.return sr
  | Error (`Msg m) ->
      let%lwt () = Lwt_io.write_line stderr (fmt "Cap file %S couldn't be loaded: %s" cap_file m) in
      Lwt.fail (Failure "cap file not found")

let run_locked = ref false

let is_running () = !run_locked

let wait_current_run_to_finish =
  let rec loop () =
    if is_running () then
      let%lwt () = Lwt_unix.sleep 1. in
      loop ()
    else
      Lwt.return_unit
  in
  loop

let update_docker_image conf =
  let repo_and_tag ~image =
    match String.split_on_char ':' image with
    | [] -> assert false
    | [image] -> (image, None)
    | image::tag -> (image, Some (String.concat ":" tag))
  in
  let get_latest_image ~image =
    let repo, tag = repo_and_tag ~image in
    let os = Server_configfile.platform_os conf in
    let arch = match Server_configfile.platform_arch conf with
      | "x86_64" -> "amd64"
      | arch -> arch
    in
    match%lwt Docker_hub.fetch_manifests ~repo ~tag with
    | Ok manifests ->
        begin match Docker_hub.digest ~os ~arch manifests with
        | Ok digest -> Lwt.return_some (image^"@"^digest)
        | Error e ->
           let e = match e with
             | `Malformed_json str -> fmt "malformed json %S" str
             | `No_corresponding_arch_found -> "no corresponding arch found"
             | `No_corresponding_os_found -> "no corresponding os found"
           in
           prerr_endline ("Something went wrong while parsing docker digest: "^e);
           Lwt.return_none
        end
    | Error e ->
       let e = match e with
         | `Api_error (response, body) ->
             Format.asprintf "response: %a, body: %a"
               Http_lwt_client.pp_response response
               (Option.pp String.pp) body
         | `Malformed_json str -> fmt "malformed json %S" str
         | `Msg str -> str
       in
       prerr_endline ("Something went wrong while fetching docker manifests: "^e);
       Lwt.return_none
  in
  let image = Server_configfile.platform_image conf in
  match String.split_on_char '@' image with
  | [] -> assert false
  | [image] ->
      begin match%lwt get_latest_image ~image with
      | Some image -> Server_configfile.set_platform_image conf image
      | None -> Lwt.fail (Failure (fmt "Could not get digest for image '%s'" image))
      end
  | [image; _old_digest] ->
      begin match%lwt get_latest_image ~image with
      | Some image -> Server_configfile.set_platform_image conf image
      | None -> prerr_endline "Defaulting to old digest"; Lwt.return_unit
      end
  | _ -> Lwt.fail (Failure (fmt "Image name '%s' is not valid" image))

let run ~debug ~cap_file ~on_finished ~conf cache workdir =
  let switches = Option.get_exn_or "no switches" (Server_configfile.ocaml_switches conf) in
  if !run_locked then
    failwith "operation locked";
  run_locked := true;
  Prometheus.Gauge.set Metrics.running 1.;
  Lwt.async begin fun () -> Lwt.finalize begin fun () ->
    let start_time = Unix.time () in
    with_stderr ~start_time workdir begin fun ~stderr ->
      try%lwt
        let timer = Oca_lib.timer_start () in
        let%lwt () = update_docker_image conf in
        let%lwt cap = get_cap ~stderr ~cap_file in
        let%lwt (opam_repo, opam_repo_commit) = get_commit_hash_default conf in
        let%lwt extra_repos = get_commit_hash_extra_repos conf in
        let switches' = switches in
        let switches = List.map (fun switch -> (switch, get_obuilder ~conf ~opam_repo ~opam_repo_commit ~extra_repos switch)) switches in
        begin match switches with
        | switch::_ ->
            let%lwt old_logdir = Oca_server.Cache.get_logdirs cache in
            let compressed = Server_configfile.enable_logs_compression conf in
            let old_logdir = List.head_opt old_logdir in
            let new_logdir = Server_workdirs.new_logdir ~compressed ~hash:opam_repo_commit ~start_time workdir in
            let%lwt () = Server_workdirs.init_base_jobs ~switches:switches' new_logdir in
            let pool = Lwt_pool.create (Server_configfile.processes conf) (fun () -> Lwt.return_unit) in
            let%lwt pkgs = Lwt_list.map_p (get_pkgs ~debug ~cap ~stderr ~conf) switches in
            let pkgs = Pkg_set.of_list (List.concat pkgs) in
            Prometheus.Gauge.set Metrics.number_of_packages (float_of_int (Pkg_set.cardinal pkgs));
            let%lwt () = Oca_lib.timer_log timer stderr "Initialization" in
            let (_, jobs) = run_jobs ~cap ~conf ~pool ~stderr new_logdir switches pkgs in
            let (_, jobs) = get_metadata ~debug ~jobs ~cap ~conf ~pool ~stderr new_logdir switch pkgs in
            let%lwt () = Lwt.join jobs in
            let%lwt () = Oca_lib.timer_log timer stderr "Operation" in
            let%lwt () = Lwt_io.write_line stderr "Finishing up…" in
            let%lwt () = move_tmpdirs_to_final ~switches:switches' new_logdir workdir in
            let%lwt () = on_finished workdir in
            let%lwt () = trigger_slack_webhooks ~stderr ~old_logdir ~new_logdir conf in
            Oca_lib.timer_log timer stderr "Clean up"
        | [] ->
            Lwt_io.write_line stderr "No switches."
        end
      with
      | exc ->
          let%lwt () = Lwt_io.write_line stderr ("Exception: "^Printexc.to_string exc^".") in
          let%lwt () = Lwt_io.write stderr (Printexc.get_backtrace ()) in
          let%lwt () = Lwt_io.flush stderr in
          Lwt.return (prerr_endline "The current run failed unexpectedly. Please check the latest log using: opam-health-check log")
    end
  end (fun () -> run_locked := false; Prometheus.Gauge.set Metrics.running 0.; Lwt.return_unit) end;
  Lwt.return_unit
