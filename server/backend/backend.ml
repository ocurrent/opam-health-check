open Lwt.Infix

type t = Server_workdirs.t

let cache = Oca_server.Cache.create ()

let get_compilers logdir =
  Server_workdirs.logdir_get_compilers logdir >|= fun compilers ->
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
  Server_workdirs.goodfiles ~switch:comp logdir >>= fun good_files ->
  Server_workdirs.partialfiles ~switch:comp logdir >>= fun partial_files ->
  Server_workdirs.badfiles ~switch:comp logdir >>= fun bad_files ->
  Server_workdirs.notavailablefiles ~switch:comp logdir >>= fun notavailable_files ->
  Server_workdirs.internalfailurefiles ~switch:comp logdir >|= fun internalfailure_files ->
  List.iter (pkg_update ~pool pkg_tbl logdir comp Intf.State.Good) good_files;
  List.iter (pkg_update ~pool pkg_tbl logdir comp Intf.State.Partial) partial_files;
  List.iter (pkg_update ~pool pkg_tbl logdir comp Intf.State.Bad) bad_files;
  List.iter (pkg_update ~pool pkg_tbl logdir comp Intf.State.NotAvailable) notavailable_files;
  List.iter (pkg_update ~pool pkg_tbl logdir comp Intf.State.InternalFailure) internalfailure_files

let add_pkg full_name instances acc =
  let pkg = Intf.Pkg.name (Intf.Pkg.create ~full_name ~instances:[] ~opam:OpamFile.OPAM.empty ~revdeps:0) in (* TODO: Remove this horror *)
  acc >>= fun acc ->
  Oca_server.Cache.get_opam cache pkg >>= fun opam ->
  Oca_server.Cache.get_revdeps cache full_name >|= fun revdeps ->
  Intf.Pkg.create ~full_name ~instances ~opam ~revdeps :: acc

let get_pkgs ~pool ~compilers logdir =
  let pkg_tbl = Pkg_tbl.create 10_000 in
  Lwt_list.iter_s (fill_pkgs_from_dir ~pool pkg_tbl logdir) compilers >>= fun () ->
  Pkg_tbl.fold add_pkg pkg_tbl Lwt.return_nil >|=
  List.sort Intf.Pkg.compare

let get_log _ ~logdir ~comp ~state ~pkg =
  Oca_server.Cache.get_pkgs ~logdir cache >>= fun pkgs ->
  let pkg = List.find (fun p -> String.equal pkg (Intf.Pkg.full_name p)) pkgs in
  let instance = List.find (fun inst -> Intf.Compiler.equal comp (Intf.Instance.compiler inst) && Intf.State.equal state (Intf.Instance.state inst)) (Intf.Pkg.instances pkg) in
  Intf.Instance.content instance

let get_opams workdir =
  let dir = Server_workdirs.opamsdir workdir in
  Oca_lib.get_files dir >>= fun files ->
  let opams = Oca_server.Cache.Opams_cache.create 10_000 in
  Lwt_list.iter_s begin fun pkg ->
    let file = Server_workdirs.opamfile ~pkg workdir in
    Lwt_io.with_file ~mode:Lwt_io.Input (Fpath.to_string file) (Lwt_io.read ?count:None) >|= fun content ->
    let content = try OpamFile.OPAM.read_from_string content with _ -> OpamFile.OPAM.empty in
    Oca_server.Cache.Opams_cache.add opams pkg content
  end files >|= fun () ->
  opams

let get_revdeps workdir =
  let dir = Server_workdirs.revdepsdir workdir in
  Oca_lib.get_files dir >>= fun files ->
  let revdeps = Oca_server.Cache.Revdeps_cache.create 10_000 in
  Lwt_list.iter_s begin fun pkg ->
    let file = Server_workdirs.revdepsfile ~pkg workdir in
    Lwt_io.with_file ~mode:Lwt_io.Input (Fpath.to_string file) (Lwt_io.read ?count:None) >|= fun content ->
    let content = String.split_on_char '\n' content in
    let content = List.hd content in
    let content = int_of_string content in
    Oca_server.Cache.Revdeps_cache.add revdeps pkg content
  end files >|= fun () ->
  revdeps

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
    ~compilers:(fun logdir -> get_compilers logdir)
    ~logdirs:(fun () -> Server_workdirs.logdirs workdir)
    ~opams:(fun () -> get_opams workdir)
    ~revdeps:(fun () -> get_revdeps workdir)
    ~html_diff:Oca_server.Html.get_diff

let run_action_loop ~conf ~run_trigger f =
  let rec loop () =
    Lwt.catch begin fun () ->
      let regular_run =
        let run_interval = Server_configfile.auto_run_interval conf * 60 * 60 in
        if run_interval > 0 then
          Lwt_unix.sleep (float_of_int run_interval) >>= fun () ->
          Check.wait_current_run_to_finish ()
        else
          fst (Lwt.wait ())
      in
      let manual_run = Lwt_mvar.take run_trigger in
      Lwt.pick [regular_run; manual_run] >>= fun () ->
      f ()
    end begin fun e ->
      let msg = Printexc.to_string e in
      Lwt_io.write_line Lwt_io.stderr ("Exception raised in action loop: "^msg)
    end >>= loop
  in
  loop ()

let start conf workdir =
  let port = Server_configfile.admin_port conf in
  let on_finished = cache_clear_and_init in
  let run_trigger = Lwt_mvar.create_empty () in
  let callback = Admin.callback ~on_finished ~conf ~run_trigger workdir in
  cache_clear_and_init workdir;
  Mirage_crypto_rng_lwt.initialize ();
  Admin.create_admin_key workdir >|= fun () ->
  let task () =
    Lwt.join [
      tcp_server port callback;
      run_action_loop ~conf ~run_trigger (fun () -> Check.run ~on_finished ~conf cache workdir);
    ]
  in
  (workdir, task)
