open Containers

module Pkgs = Map.Make (String)

type state = Good | Bad | Nop

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

let pkg_update ~len ~idx def = function
  | None -> Some (List.init len (fun i -> if i = idx then def else Nop))
  | Some l -> Some (List.set_at_idx idx def l)

let get_pkgs_from_dir ~len pkgs idx dir =
  let good_files = get_files (Filename.concat dir "good") in
  let bad_files = get_files (Filename.concat dir "bad") in
  let aux def pkgs pkg = Pkgs.update pkg (pkg_update ~len ~idx def) pkgs in
  let pkgs = List.fold_left (aux Good) pkgs good_files in
  List.fold_left (aux Bad) pkgs bad_files

let get_pkgs dirs =
  let len = List.length dirs in
  let pkgs = List.foldi (get_pkgs_from_dir ~len) Pkgs.empty dirs in
  let pkgs = Pkgs.bindings pkgs in
  List.sort (fun (x, _) (y, _) -> String.compare x y) pkgs

let state_to_html =
  let open Tyxml.Html in
  let td c = td ~a:[a_style ("border: 2px solid black; background-color: "^c^"; text-align: center;")] in
  function
  | Good -> td "green" [pcdata "☑"]
  | Bad -> td "red" [pcdata "☒"]
  | Nop -> td "grey" [pcdata "☐"]

let pkg_to_html (pkg, instances) =
  let open Tyxml.Html in
  let td = td ~a:[a_style "border: 2px solid black;"] in
  td [pcdata pkg] :: List.map state_to_html instances

let print_html dirs pkgs =
  let open Tyxml.Html in
  let table = table ~a:[a_style "border-collapse: collapse;"] in
  let th = th ~a:[a_style "border: 2px solid black; test-align: center; width: 20em;"] in
  let dirs = th [] :: List.map (fun dir -> th [pcdata (Filename.basename dir)]) dirs in
  let pkgs = List.map (fun pkg -> tr (pkg_to_html pkg)) pkgs in
  let doc = table ~thead:(thead [tr dirs]) pkgs in
  let doc = html (head (title (pcdata "title")) [meta ~a:[a_charset "utf-8"] ()]) (body [doc]) in
  Format.printf "%a\n" (pp ()) doc

let () =
  match Sys.argv with
  | [|_; logdir|] ->
      let dirs = get_dirs logdir in
      let pkgs = get_pkgs dirs in
      print_html dirs pkgs
  | _ ->
      prerr_endline "Read the code and try again";
      exit 1
