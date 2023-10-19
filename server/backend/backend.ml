type t = Server_workdirs.t

let cache = Oca_server.Cache.create ()

let get_compilers logdir =
  let compilers = Server_workdirs.logdir_get_compilers logdir in
  List.sort Intf.Compiler.compare compilers

module Pkg_tbl = Hashtbl.Make (String)

let pkg_update ~pool pkg_tbl logdir comp state pkg =
  let get_content () = Lwt_pool.use pool begin fun () ->
    Server_workdirs.logdir_get_content ~comp ~state ~pkg logdir
  end in
  let content = Intf.Log.create get_content in
  let instances =
    match Pkg_tbl.find_opt pkg_tbl pkg with
    | Some instances -> Intf.Instance.create comp state content :: instances
    | None -> [Intf.Instance.create comp state content]
  in
  Pkg_tbl.replace pkg_tbl pkg instances

let fill_pkgs_from_dir ~pool pkg_tbl logdir comp =
  let good_files = Server_workdirs.goodfiles ~switch:comp logdir in
  let partial_files = Server_workdirs.partialfiles ~switch:comp logdir in
  let bad_files = Server_workdirs.badfiles ~switch:comp logdir in
  let notavailable_files = Server_workdirs.notavailablefiles ~switch:comp logdir in
  let internalfailure_files = Server_workdirs.internalfailurefiles ~switch:comp logdir in
  List.iter (pkg_update ~pool pkg_tbl logdir comp Intf.State.Good) good_files;
  List.iter (pkg_update ~pool pkg_tbl logdir comp Intf.State.Partial) partial_files;
  List.iter (pkg_update ~pool pkg_tbl logdir comp Intf.State.Bad) bad_files;
  List.iter (pkg_update ~pool pkg_tbl logdir comp Intf.State.NotAvailable) notavailable_files;
  List.iter (pkg_update ~pool pkg_tbl logdir comp Intf.State.InternalFailure) internalfailure_files;
  ()

let add_pkg full_name instances acc =
  let pkg = Intf.Pkg.name (Intf.Pkg.create ~full_name ~instances:[] ~opam:OpamFile.OPAM.empty ~revdeps:0) in (* TODO: Remove this horror *)
  let%lwt acc = acc in
  let%lwt opam = Oca_server.Cache.get_opam cache pkg in
  let%lwt revdeps = Oca_server.Cache.get_revdeps cache full_name in
  Lwt.return (Intf.Pkg.create ~full_name ~instances ~opam ~revdeps :: acc)

let get_pkgs ~pool ~compilers logdir =
  let pkg_tbl = Pkg_tbl.create 10_000 in
  List.iter (fill_pkgs_from_dir ~pool pkg_tbl logdir) compilers;
  let%lwt pkgs = Pkg_tbl.fold add_pkg pkg_tbl Lwt.return_nil in
  Lwt.return (List.sort Intf.Pkg.compare pkgs)

let get_log _ ~logdir ~comp ~state ~pkg =
  let%lwt pkgs = Oca_server.Cache.get_pkgs ~logdir cache in
  match List.find_opt (fun p -> String.equal pkg (Intf.Pkg.full_name p)) pkgs with
  | None -> Lwt.return_none
  | Some pkg ->
      let is_instance inst =
        Intf.Compiler.equal comp (Intf.Instance.compiler inst) &&
        Intf.State.equal state (Intf.Instance.state inst)
      in
      match List.find_opt is_instance (Intf.Pkg.instances pkg) with
      | None -> Lwt.return_none
      | Some instance ->
          let%lwt content = Intf.Instance.content instance in
          Lwt.return (Some content)

let get_opams workdir =
  let dir = Server_workdirs.opamsdir workdir in
  let%lwt files = Oca_lib.get_files dir in
  let opams = Oca_server.Cache.Opams_cache.empty in
  let%lwt opams =
    Lwt_list.fold_left_s begin fun opams pkg ->
      let file = Server_workdirs.opamfile ~pkg workdir in
      let%lwt content = Lwt_io.with_file ~mode:Lwt_io.Input (Fpath.to_string file) (Lwt_io.read ?count:None) in
      let content = try OpamFile.OPAM.read_from_string content with _ -> OpamFile.OPAM.empty in
      Lwt.return (Oca_server.Cache.Opams_cache.add pkg content opams)
    end opams files
  in
  Lwt.return opams

let get_revdeps workdir =
  let dir = Server_workdirs.revdepsdir workdir in
  let%lwt files = Oca_lib.get_files dir in
  let revdeps = Oca_server.Cache.Revdeps_cache.empty in
  let%lwt revdeps =
    Lwt_list.fold_left_s begin fun revdeps pkg ->
      let file = Server_workdirs.revdepsfile ~pkg workdir in
      let%lwt content = Lwt_io.with_file ~mode:Lwt_io.Input (Fpath.to_string file) (Lwt_io.read ?count:None) in
      let content = String.split_on_char '\n' content in
      let content = List.hd content in
      let content = int_of_string content in
      Lwt.return (Oca_server.Cache.Revdeps_cache.add pkg content revdeps)
    end revdeps files
  in
  Lwt.return revdeps

(* TODO: Deduplicate with Server.tcp_server *)
let tcp_server port callback =
  Cohttp_lwt_unix.Server.create
    ~on_exn:(fun _ -> ())
    ~mode:(`TCP (`Port port))
    (Cohttp_lwt_unix.Server.make ~callback ())

let cache_clear_and_init workdir =
  let pool = Lwt_pool.create 64 (fun () -> Lwt.return_unit) in
  Oca_server.Cache.clear_and_init
    cache
    ~pkgs:(fun ~compilers logdir -> get_pkgs ~pool ~compilers logdir)
    ~compilers:(fun logdir -> Lwt.return (get_compilers logdir))
    ~logdirs:(fun () -> Server_workdirs.logdirs workdir)
    ~opams:(fun () -> get_opams workdir)
    ~revdeps:(fun () -> get_revdeps workdir)

let run_action_loop ~conf ~run_trigger f =
  let rec loop () =
    let%lwt () =
      try%lwt
        let regular_run =
          let run_interval = Server_configfile.auto_run_interval conf * 60 * 60 in
          if run_interval > 0 then
            let rec loop t =
              Prometheus.Gauge.set Metrics.seconds_until_next_run (float_of_int t);
              match t with
              | 0 -> Check.wait_current_run_to_finish ()
              | n -> let%lwt () = Lwt_unix.sleep 60.0 in loop (n - 60)
            in
              loop run_interval
          else
            fst (Lwt.wait ())
        in
        let manual_run = Lwt_mvar.take run_trigger in
        let%lwt () = Lwt.pick [regular_run; manual_run] in
        f ()
      with e ->
        let msg = Printexc.to_string e in
        let%lwt () = Lwt_io.write_line Lwt_io.stderr ("Exception raised in action loop: "^msg) in
        Lwt_io.write_line Lwt_io.stderr (Printexc.get_backtrace ())
    in
    loop ()
  in
  loop ()

let start ~debug ~cap_file conf workdir =
  let port = Server_configfile.admin_port conf in
  let on_finished = cache_clear_and_init in
  let run_trigger = Lwt_mvar.create_empty () in
  let callback = Admin.callback ~on_finished ~conf ~run_trigger workdir in
  Lwt.ignore_result (cache_clear_and_init workdir);
  Mirage_crypto_rng_lwt.initialize (module Mirage_crypto_rng.Fortuna);
  let%lwt () = Admin.create_admin_key workdir in
  let task () =
    Lwt.join [
      tcp_server port callback;
      run_action_loop ~conf ~run_trigger (fun () -> Check.run ~debug ~cap_file ~on_finished ~conf cache workdir);
    ]
  in
  Lwt.return (workdir, task)
