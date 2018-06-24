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

type t = {
  mutable pkgsinfo : Obi.Index.pkg list Lwt.t;
  html_tbl : string Html_cache.t;
  mutable pkgs : Intf.Pkg.t list Lwt.t;
  mutable compilers : Intf.Compiler.t list Lwt.t;
}

let create () = {
  pkgsinfo = Lwt.return_nil;
  html_tbl = Html_cache.create 32;
  pkgs = Lwt.return_nil;
  compilers = Lwt.return_nil;
}

let clear_and_init self pkgs compilers =
  self.pkgsinfo <- Metainfo.get_pkgsinfo ();
  self.compilers <- compilers;
  self.pkgs <- pkgs;
  Html_cache.clear self.html_tbl

let get_html self query =
  self.pkgs >>= fun pkgs ->
  let html = Html.get_html query pkgs in
  Html_cache.add self.html_tbl query html;
  Lwt.return html

let get_html self query =
  match Html_cache.find_opt self.html_tbl query with
  | Some html -> Lwt.return html
  | None -> get_html self query

let get_compilers self =
  self.compilers

let get_pkgsinfo self =
  self.pkgsinfo
