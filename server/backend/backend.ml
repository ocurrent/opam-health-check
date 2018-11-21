open Lwt.Infix

type t = Server_workdirs.t

let cache = Oca_server.Cache.create ()

let get_files dirname =
  Lwt_unix.opendir (Fpath.to_string dirname) >>= fun dir ->
  let rec aux files =
    Lwt.catch begin fun () ->
      Lwt_unix.readdir dir >>= fun file ->
      if Fpath.is_rel_seg file then
        aux files
      else
        aux (file :: files)
    end begin function
    | End_of_file -> Lwt.return files
    | exn -> Lwt.fail exn
    end
  in
  aux [] >>= fun files ->
  Lwt_unix.closedir dir >|= fun () ->
  files

let is_directory dir file =
  if Sys.is_directory (Fpath.to_string (Fpath.add_seg dir file)) then
    Some (Intf.Compiler.from_string file)
  else
    None

let get_compilers workdir =
  let dir = Server_workdirs.logdir workdir in
  get_files dir >|= fun files ->
  let dirs = List.filter_map (is_directory dir) files in
  List.sort Intf.Compiler.compare dirs

module Pkg_tbl = Hashtbl.Make (String)

let pkg_update pkg_tbl comp state pkg =
  let instances =
    match Pkg_tbl.find_opt pkg_tbl pkg with
    | Some instances -> Intf.Instance.create comp state :: instances
    | None -> [Intf.Instance.create comp state]
  in
  Pkg_tbl.replace pkg_tbl pkg instances

let fill_pkgs_from_dir pkg_tbl workdir comp =
  get_files (Server_workdirs.gooddir ~switch:comp workdir) >>= fun good_files ->
  get_files (Server_workdirs.partialdir ~switch:comp workdir) >>= fun partial_files ->
  get_files (Server_workdirs.baddir ~switch:comp workdir) >|= fun bad_files ->
  List.iter (pkg_update pkg_tbl comp Intf.State.Good) good_files;
  List.iter (pkg_update pkg_tbl comp Intf.State.Partial) partial_files;
  List.iter (pkg_update pkg_tbl comp Intf.State.Bad) bad_files

let add_pkg obi full_name instances acc =
  let pkg = Intf.Pkg.name (Intf.Pkg.create ~full_name ~instances:[] ~maintainers:[]) in (* TODO: Remove this horror *)
  let maintainers =
    match List.find_opt (fun pkg' -> String.equal pkg'.Obi.Index.name pkg) obi with
    | Some obi -> obi.Obi.Index.maintainers
    | None -> []
  in
  Intf.Pkg.create ~full_name ~instances ~maintainers :: acc

let get_pkgs workdir =
  let pkg_tbl = Pkg_tbl.create 10_000 in
  Oca_server.Cache.get_pkgsinfo cache >>= fun obi ->
  Oca_server.Cache.get_compilers cache >>= fun compilers ->
  Lwt_list.iter_s (fill_pkgs_from_dir pkg_tbl workdir) compilers >|= fun () ->
  let pkgs = Pkg_tbl.fold (add_pkg obi) pkg_tbl [] in
  List.sort Intf.Pkg.compare pkgs

let get_log workdir ~comp ~state ~pkg =
  let comp = Intf.Compiler.to_string comp in
  let state = Intf.State.to_string state in
  if not (Oca_lib.is_valid_filename pkg) then
    failwith "Wrong filename";
  let file = Fpath.(to_string (v comp/state/pkg)) in
  let file = Server_workdirs.file_from_logdir ~file workdir in
  Lwt_io.with_file ~mode:Lwt_io.Input (Fpath.to_string file) (Lwt_io.read ?count:None)

(* TODO: Deduplicate with Server.tcp_server *)
let tcp_server port callback =
  Cohttp_lwt_unix.Server.create
    ~on_exn:(fun _ -> ())
    ~mode:(`TCP (`Port port))
    (Cohttp_lwt_unix.Server.make ~callback ())

let cache_clear_and_init workdir =
  Oca_server.Cache.clear_and_init cache (fun ()-> get_pkgs workdir) (fun () -> get_compilers workdir)

let run_action_loop ~run_trigger f =
  let two_days = float_of_int (48 * 60 * 60) in
  let rec loop () =
    Lwt.choose [Lwt_unix.sleep two_days; Lwt_mvar.take run_trigger] >>= f >>= loop
  in
  loop ()

let start conf workdir =
  let port = Server_configfile.admin_port conf in
  let on_finished = cache_clear_and_init in
  let run_trigger = Lwt_mvar.create_empty () in
  let callback = Admin.callback ~on_finished ~conf ~run_trigger workdir in
  cache_clear_and_init workdir;
  get_compilers workdir >>= Server_configfile.set_ocaml_switches conf >>= fun () ->
  Nocrypto_entropy_lwt.initialize () >>= fun () ->
  Admin.create_admin_key workdir >|= fun () ->
  let task () =
    Lwt.join [
      tcp_server port callback;
      run_action_loop ~run_trigger (fun () -> Check.run ~on_finished ~conf workdir);
    ]
  in
  (workdir, task)
