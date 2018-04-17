open Containers

module Pkgs = Map.Make (String)

type state = Good | Bad

type comp = string
type pkg = string
type instance_name = string
type file = string
type pkgs = (pkg * (instance_name * (file * state)) list) list

let get_files dirname =
  let dir = Unix.opendir dirname in
  let rec aux files = match Unix.readdir dir with
    | "." | ".." -> aux files
    | file -> aux (file :: files)
    | exception End_of_file -> files
  in
  let files = aux [] in
  Unix.closedir dir;
  files

let is_directory dir file =
  Sys.is_directory (Filename.concat dir file)

let get_dirs dir =
  let files = get_files dir in
  let dirs = List.filter (is_directory dir) files in
  List.sort OpamVersionCompare.compare dirs

let pkg_update ~comp ~file v = function
  | None -> Some [(comp, (file, v))]
  | Some l -> Some ((comp, (file, v)) :: l)

let get_pkgs_from_dir ~logdir pkgs comp =
  let dir = Filename.concat logdir comp in
  let gooddir = Filename.concat dir "good" in
  let baddir = Filename.concat dir "bad" in
  let good_files = get_files gooddir in
  let bad_files = get_files baddir in
  let aux dir v pkgs pkg =
    Pkgs.update pkg (pkg_update ~comp ~file:(dir^pkg) v) pkgs
  in
  let pkgs = List.fold_left (aux ("/"^comp^"/good/") Good) pkgs good_files in
  List.fold_left (aux ("/"^comp^"/bad/") Bad) pkgs bad_files

let get_pkgs ~logdir compilers =
  let pkgs = List.fold_left (get_pkgs_from_dir ~logdir) Pkgs.empty compilers in
  let pkgs = Pkgs.bindings pkgs in
  List.sort (fun (x, _) (y, _) -> OpamVersionCompare.compare x y) pkgs

let instance_to_html instances comp =
  let open Tyxml.Html in
  let td c = td ~a:[a_class ["result-col"]; a_style ("background-color: "^c^";")] in
  match List.Assoc.get ~eq:String.equal comp instances with
  | Some (_path, Good) -> td "green" [pcdata "☑"]
  | Some (path, Bad) -> td "red" [a ~a:[a_href path] [pcdata "☒"]]
  | None -> td "grey" [pcdata "☐"]

let is_pkg_available instances compilers =
  List.exists (fun dir -> List.Assoc.mem ~eq:String.equal dir instances) compilers

let pkg_to_html compilers (pkg, instances) =
  let open Tyxml.Html in
  if is_pkg_available instances compilers then
    Some (tr (td [pcdata pkg] :: List.map (instance_to_html instances) compilers))
  else
    None

let get_html compilers pkgs =
  let open Tyxml.Html in
  let col_width = string_of_int (100 / List.length compilers) in
  let pkgs = List.filter_map (pkg_to_html compilers) pkgs in
  let dirs = th [] :: List.map (fun dir -> th ~a:[a_class ["result-col"]] [pcdata (Filename.basename dir)]) compilers in
  let title = title (pcdata "opam-check-all") in
  let charset = meta ~a:[a_charset "utf-8"] () in
  let style_table = pcdata "table {border-collapse: collapse; min-width: 100%;}" in
  let style_col = pcdata (".result-col {text-align: center; width: "^col_width^"%;}") in
  let style_case = pcdata "td, th {border: 2px solid black;}" in
  let head = head title [charset; style [style_table; style_col; style_case]] in
  let doc = table ~thead:(thead [tr dirs]) pkgs in
  let doc = html head (body [doc]) in
  Format.sprintf "%a\n" (pp ()) doc

let comp_from_string x = x
