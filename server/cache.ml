open Containers
open Lwt.Infix

module Hashtbl = Hashtbl.Make (struct
    type t = Diff.query
    let hash = Hashtbl.hash
    let equal x y =
      List.equal Diff.comp_equal x.Diff.compilers y.Diff.compilers &&
      List.equal Diff.comp_equal x.Diff.show_available y.Diff.show_available
  end)

(* TODO: Also cache pkgs ? *)
let hashtbl = Hashtbl.create 32

let clear () =
  Hashtbl.clear hashtbl

let get_html workdir query =
  Diff.get_pkgs workdir query.Diff.compilers >>= fun pkgs ->
  let html = Diff.get_html query pkgs in
  Hashtbl.add hashtbl query html;
  Lwt.return html

let get_html workdir query =
  match Hashtbl.find_opt hashtbl query with
  | Some html -> Lwt.return html
  | None -> get_html workdir query
