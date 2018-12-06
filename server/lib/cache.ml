open Lwt.Infix

open Intf

module Html_cache = Hashtbl.Make (struct
    type t = Html.query
    let hash = Hashtbl.hash
    let equal {Html.available_compilers; compilers; show_available; show_failures_only; show_diff_only; show_latest_only; maintainers} y =
      List.equal Compiler.equal available_compilers y.Html.available_compilers &&
      List.equal Compiler.equal compilers y.Html.compilers &&
      List.equal Compiler.equal show_available y.Html.show_available &&
      Bool.equal show_failures_only y.Html.show_failures_only &&
      Bool.equal show_diff_only y.Html.show_diff_only &&
      Bool.equal show_latest_only y.Html.show_latest_only &&
      String.equal (fst maintainers) (fst y.Html.maintainers)
  end)

module Maintainers_cache = Hashtbl.Make (String)

type t = {
  html_tbl : string Html_cache.t;
  mutable pkgs : Intf.Pkg.t list Lwt.t;
  mutable compilers : Intf.Compiler.t list Lwt.t;
  mutable maintainers : string list Maintainers_cache.t Lwt.t;
}

let create () = {
  html_tbl = Html_cache.create 32;
  pkgs = Lwt.return_nil;
  compilers = Lwt.return_nil;
  maintainers = Lwt.return (Maintainers_cache.create 0);
}

let clear_and_init self pkgs compilers maintainers =
  self.maintainers <- maintainers ();
  self.compilers <- compilers ();
  self.pkgs <- pkgs ();
  Html_cache.clear self.html_tbl

let get_html ~conf self query =
  self.pkgs >>= fun pkgs ->
  let html = Html.get_html ~conf query pkgs in
  Html_cache.add self.html_tbl query html;
  Lwt.return html

let get_html ~conf self query =
  match Html_cache.find_opt self.html_tbl query with
  | Some html -> Lwt.return html
  | None -> get_html ~conf self query

let get_compilers self =
  self.compilers

let get_maintainers self k =
  self.maintainers >|= fun maintainers ->
  Option.get_or ~default:[] (Maintainers_cache.find_opt maintainers k)
