open Containers

module Pkgs = Map.Make (String)

type state = Good | Bad of string | Nop

type dir = string
type pkgs = (string * state list) list

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

let get_dirs dir =
  let files = get_files dir in
  let files = List.map (Filename.concat dir) files in
  let dirs = List.filter Sys.is_directory files in
  List.sort String.compare dirs

let pkg_update ~dir ~pkg ~len ~idx def = function
  | None -> Some (List.init len (fun i -> if i = idx then def ~dir ~pkg else Nop))
  | Some l -> Some (List.set_at_idx idx (def ~dir ~pkg) l)

let get_pkgs_from_dir ~len pkgs idx dir =
  let good_files = get_files (Filename.concat dir "good") in
  let bad_files = get_files (Filename.concat dir "bad") in
  let aux def pkgs pkg =
    Pkgs.update pkg (pkg_update ~dir ~pkg ~len ~idx def) pkgs
  in
  let pkgs = List.fold_left (aux (fun ~dir ~pkg -> Good)) pkgs good_files in
  List.fold_left (aux (fun ~dir ~pkg -> Bad (Filename.concat (Filename.concat (Filename.basename dir) "bad") pkg))) pkgs bad_files

let get_pkgs dirs =
  let len = List.length dirs in
  let pkgs = List.foldi (get_pkgs_from_dir ~len) Pkgs.empty dirs in
  let pkgs = Pkgs.bindings pkgs in
  List.sort (fun (x, _) (y, _) -> String.compare x y) pkgs

let state_to_html =
  let open Tyxml.Html in
  let td c = td ~a:[a_class ["result-col"]; a_style ("background-color: "^c^";")] in
  function
  | Good -> td "green" [pcdata "☑"]
  | Bad path -> td "red" [a ~a:[a_href path] [pcdata "☒"]]
  | Nop -> td "grey" [pcdata "☐"]

let pkg_to_html (pkg, instances) =
  let open Tyxml.Html in
  td [pcdata pkg] :: List.map state_to_html instances

let get_html dirs pkgs =
  let open Tyxml.Html in
  let col_width = string_of_int (100 / List.length dirs) in
  let dirs = th [] :: List.map (fun dir -> th ~a:[a_class ["result-col"]] [pcdata (Filename.basename dir)]) dirs in
  let pkgs = List.map (fun pkg -> tr (pkg_to_html pkg)) pkgs in
  let title = title (pcdata "opam-check-all") in
  let charset = meta ~a:[a_charset "utf-8"] () in
  let style_table = pcdata "table {border-collapse: collapse; min-width: 100%;}" in
  let style_col = pcdata (".result-col {text-align: center; width: "^col_width^"%;}") in
  let style_case = pcdata "td, th {border: 2px solid black;}" in
  let head = head title [charset; style [style_table; style_col; style_case]] in
  let doc = table ~thead:(thead [tr dirs]) pkgs in
  let doc = html head (body [doc]) in
  Format.sprintf "%a\n" (pp ()) doc
