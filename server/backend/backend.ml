open Lwt.Infix

type t = Server_workdirs.t

let cache = Oca_server.Cache.create ()

let is_directory dir file =
  if Sys.is_directory (Fpath.to_string (Fpath.add_seg dir file)) then
    Some (Intf.Compiler.from_string file)
  else
    None

let get_compilers ~old workdir =
  let dir = Server_workdirs.logdir ~old workdir in
  Oca_lib.get_files dir >|= fun files ->
  let dirs = List.filter_map (is_directory dir) files in
  List.sort Intf.Compiler.compare dirs

module Pkg_tbl = Hashtbl.Make (String)

let pkg_update ~old ~pool pkg_tbl workdir comp state pkg =
  let file =
    let comp = Intf.Compiler.to_string comp in
    let state = Intf.State.to_string state in
    Fpath.(to_string (v comp/state/pkg))
  in
  let file = Server_workdirs.file_from_logdir ~old ~file workdir in
  let get_content () = Lwt_pool.use pool begin fun () ->
    Lwt_io.with_file ~mode:Lwt_io.Input (Fpath.to_string file) (Lwt_io.read ?count:None)
  end in
  let content =
    if old then Intf.Log.unstored get_content else Intf.Log.compressed (get_content ())
  in
  let instances =
    match Pkg_tbl.find_opt pkg_tbl pkg with
    | Some instances -> Intf.Instance.create comp state content :: instances
    | None -> [Intf.Instance.create comp state content]
  in
  Pkg_tbl.replace pkg_tbl pkg instances

let fill_pkgs_from_dir ~old ~pool pkg_tbl workdir comp =
  Oca_lib.get_files (Server_workdirs.gooddir ~old ~switch:comp workdir) >>= fun good_files ->
  Oca_lib.get_files (Server_workdirs.partialdir ~old ~switch:comp workdir) >>= fun partial_files ->
  Oca_lib.get_files (Server_workdirs.baddir ~old ~switch:comp workdir) >>= fun bad_files ->
  Oca_lib.get_files (Server_workdirs.notavailabledir ~old ~switch:comp workdir) >>= fun notavailable_files ->
  Oca_lib.get_files (Server_workdirs.internalfailuredir ~old ~switch:comp workdir) >|= fun internalfailure_files ->
  List.iter (pkg_update ~old ~pool pkg_tbl workdir comp Intf.State.Good) good_files;
  List.iter (pkg_update ~old ~pool pkg_tbl workdir comp Intf.State.Partial) partial_files;
  List.iter (pkg_update ~old ~pool pkg_tbl workdir comp Intf.State.Bad) bad_files;
  List.iter (pkg_update ~old ~pool pkg_tbl workdir comp Intf.State.NotAvailable) notavailable_files;
  List.iter (pkg_update ~old ~pool pkg_tbl workdir comp Intf.State.InternalFailure) internalfailure_files

let add_pkg full_name instances acc =
  let pkg = Intf.Pkg.name (Intf.Pkg.create ~full_name ~instances:[] ~maintainers:[] ~revdeps:0) in (* TODO: Remove this horror *)
  acc >>= fun acc ->
  Oca_server.Cache.get_maintainers cache pkg >>= fun maintainers ->
  Oca_server.Cache.get_revdeps cache full_name >|= fun revdeps ->
  Intf.Pkg.create ~full_name ~instances ~maintainers ~revdeps :: acc

let get_pkgs ~old workdir =
  let pkg_tbl = Pkg_tbl.create 10_000 in
  Oca_server.Cache.get_compilers ~old cache >>= fun compilers ->
  let pool = Lwt_pool.create 64 (fun () -> Lwt.return_unit) in
  Lwt_list.iter_s (fill_pkgs_from_dir ~old ~pool pkg_tbl workdir) compilers >>= fun () ->
  Pkg_tbl.fold add_pkg pkg_tbl Lwt.return_nil >|=
  List.sort Intf.Pkg.compare

let get_log _ ~old ~comp ~state ~pkg =
  Oca_server.Cache.get_pkgs ~old cache >>= fun pkgs ->
  let pkg = List.find (fun p -> String.equal pkg (Intf.Pkg.full_name p)) pkgs in
  let instance = List.find (fun inst -> Intf.Compiler.equal comp (Intf.Instance.compiler inst) && Intf.State.equal state (Intf.Instance.state inst)) (Intf.Pkg.instances pkg) in
  Intf.Instance.content instance

let get_maintainers workdir =
  let dir = Server_workdirs.maintainersdir workdir in
  Oca_lib.get_files dir >>= fun files ->
  let maintainers = Oca_server.Cache.Maintainers_cache.create 10_000 in
  Lwt_list.iter_s begin fun pkg ->
    let file = Server_workdirs.maintainersfile ~pkg workdir in
    Lwt_io.with_file ~mode:Lwt_io.Input (Fpath.to_string file) (Lwt_io.read ?count:None) >|= fun content ->
    let content = String.split_on_char '\n' content in
    Oca_server.Cache.Maintainers_cache.add maintainers pkg content
  end files >|= fun () ->
  maintainers

let get_revdeps workdir =
  let dir = Server_workdirs.revdepsdir workdir in
  Oca_lib.get_files dir >>= fun files ->
  let revdeps = Oca_server.Cache.Revdeps_cache.create 10_000 in
  Lwt_list.iter_s begin fun pkg ->
    let file = Server_workdirs.revdepsfile ~pkg workdir in
    Lwt_io.with_file ~mode:Lwt_io.Input (Fpath.to_string file) (Lwt_io.read ?count:None) >|= fun content ->
    let content = int_of_string content in
    Oca_server.Cache.Revdeps_cache.add revdeps pkg content
  end files >|= fun () ->
  revdeps

let get_html_diff ~conf =
  Oca_server.Cache.get_diff cache >|= Oca_server.Html.get_diff ~conf

(* TODO: Deduplicate with Server.tcp_server *)
let tcp_server port callback =
  Cohttp_lwt_unix.Server.create
    ~on_exn:(fun _ -> ())
    ~mode:(`TCP (`Port port))
    (Cohttp_lwt_unix.Server.make ~callback ())

let cache_clear_and_init conf workdir =
  Oca_server.Cache.clear_and_init
    cache
    ~pkgs:(fun ~old -> get_pkgs ~old workdir)
    ~compilers:(fun ~old -> get_compilers ~old workdir)
    ~maintainers:(fun () -> get_maintainers workdir)
    ~revdeps:(fun () -> get_revdeps workdir)
    ~html_diff:(fun () -> get_html_diff ~conf)

let run_action_loop ~conf ~run_trigger f =
  let rec loop () =
    Lwt.catch begin fun () ->
      let regular_run =
        let run_interval = Server_configfile.auto_run_interval conf * 60 * 60 in
        if run_interval > 0 then
          Lwt_unix.sleep (float_of_int run_interval) >>= fun () ->
          Check.wait_current_run_to_finish () >|= fun () -> false
        else
          fst (Lwt.wait ())
      in
      let manual_run = Lwt_mvar.take run_trigger in
      Lwt.pick [regular_run; manual_run] >>= fun is_retry -> f ~is_retry
    end begin fun e ->
      let msg = Printexc.to_string e in
      Lwt_io.write_line Lwt_io.stderr ("Exception raised in action loop: "^msg)
    end >>= loop
  in
  loop ()

let start conf workdir =
  let port = Server_configfile.admin_port conf in
  let on_finished = cache_clear_and_init conf in
  let run_trigger = Lwt_mvar.create_empty () in
  let callback = Admin.callback ~on_finished ~conf ~run_trigger workdir in
  cache_clear_and_init conf workdir;
  Server_configfile.set_default_ocaml_switches conf (fun () -> get_compilers ~old:false workdir) >>= fun () ->
  Nocrypto_entropy_lwt.initialize () >>= fun () ->
  Admin.create_admin_key workdir >|= fun () ->
  let task () =
    Lwt.join [
      tcp_server port callback;
      run_action_loop ~conf ~run_trigger (fun ~is_retry -> Check.run ~on_finished ~is_retry ~conf workdir);
    ]
  in
  (workdir, task)
