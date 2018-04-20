open Containers
open Lwt.Infix

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
  begin if is_default then Diff.get_dirs logdir else Lwt.return compilers end >>= fun compilers ->
  Diff.get_pkgs ~logdir compilers >>= fun pkgs ->
  let html = Diff.get_html compilers pkgs in
  Hashtbl.add hashtbl compilers html;
  if is_default then Hashtbl.add hashtbl [] html;
  Lwt.return html

let get_html ~logdir compilers =
  match Hashtbl.find_opt hashtbl compilers with
  | Some html -> Lwt.return html
  | None -> get_html ~logdir compilers
