open Lwt.Infix

type state = Good | Partial | Bad
type comp = string
type instance = (comp * state)
type pkg = {
  full_name : string;
  name : string;
}

type info = {
  maintainers : string list;
  instances : instance list;
}

let state_eq x y = match x, y with
  | Good, Good | Partial, Partial | Bad, Bad -> true
  | Good, _ | Partial, _ | Bad, _ -> false

let pkg_create full_name =
  let name =
    match String.index_opt full_name '.' with
    | Some idx -> String.sub full_name 0 idx
    | None -> failwith "packages must have a version separated by a dot"
  in
  {full_name; name}

let pkg_equal x y = String.equal x.full_name y.full_name
let pkg_compare x y = OpamVersionCompare.compare x.full_name y.full_name (* TODO: Is it ok to compare full names ? *)
let pkg_to_string x = x.full_name
let pkg_name_to_string x = x.name

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
  Sys.is_directory (Fpath.to_string (Fpath.add_seg dir file))

let get_compilers workdir =
  let dir = Server_workdirs.logdir workdir in
  get_files dir >|= fun files ->
  let dirs = List.filter (is_directory dir) files in
  List.sort OpamVersionCompare.compare dirs

let pkg_update ~comp ~update v pkg =
  let aux l = (comp, v) :: l in
  update (pkg_create pkg) aux

let fill_pkgs_from_dir workdir ~update comp =
  get_files (Server_workdirs.gooddir ~switch:comp workdir) >>= fun good_files ->
  get_files (Server_workdirs.partialdir ~switch:comp workdir) >>= fun partial_files ->
  get_files (Server_workdirs.baddir ~switch:comp workdir) >|= fun bad_files ->
  List.iter (pkg_update ~comp ~update Good) good_files;
  List.iter (pkg_update ~comp ~update Partial) partial_files;
  List.iter (pkg_update ~comp ~update Bad) bad_files

let fill_pkgs ~update workdir =
  get_compilers workdir >>= fun compilers ->
  Lwt_list.iter_s (fill_pkgs_from_dir workdir ~update) compilers

let comp_from_string x = x
let comp_to_string x = x
let comp_equal = String.equal
