open Containers

module Hashtbl = Hashtbl.Make (struct
    type t = Diff.comp list
    let hash = Hashtbl.hash
    let equal = List.equal Diff.comp_equal
  end)

(* TODO: Also cache pkgs ? *)
let hashtbl = Hashtbl.create 32

let clear () =
  Hashtbl.clear hashtbl

let get_html ~logdir compilers =
  let is_default = List.is_empty compilers in
  let compilers = if is_default then Diff.get_dirs logdir else compilers in
  let pkgs = Diff.get_pkgs ~logdir compilers in
  let html = Diff.get_html compilers pkgs in
  Hashtbl.add hashtbl compilers html;
  if is_default then Hashtbl.add hashtbl [] html;
  html

let get_html ~logdir compilers =
  match Hashtbl.find_opt hashtbl compilers with
  | Some html -> html
  | None -> get_html ~logdir compilers
