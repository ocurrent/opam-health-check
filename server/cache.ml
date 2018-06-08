open Lwt.Infix

module Html_cache = Hashtbl.Make (struct
    type t = Diff.query
    let hash = Hashtbl.hash
    let equal {Diff.compilers; show_available; show_failures_only; show_diff_only; show_latest_only; maintainers} y =
      List.equal Diff.comp_equal compilers y.Diff.compilers &&
      List.equal Diff.comp_equal show_available y.Diff.show_available &&
      Bool.equal show_failures_only y.Diff.show_failures_only &&
      Bool.equal show_diff_only y.Diff.show_diff_only &&
      Bool.equal show_latest_only y.Diff.show_latest_only &&
      String.equal (fst maintainers) (fst y.Diff.maintainers)
  end)
module Pkginfo_cache = Hashtbl.Make (String)

(* TODO: Also cache pkgs ? *)
let raw_pkginfo = ref (Metainfo.fetch_raw_metainfo ())
let html_tbl = Html_cache.create 32
let pkginfo_tbl = Pkginfo_cache.create 10_000

let clear () =
  raw_pkginfo := Metainfo.fetch_raw_metainfo ();
  Pkginfo_cache.clear pkginfo_tbl;
  Html_cache.clear html_tbl

let default_metainfo = {
  Metainfo.maintainers = [];
}

let get_pkginfo pkg =
  Option.get_or ~default:default_metainfo (Pkginfo_cache.find_opt pkginfo_tbl pkg)

let get_html workdir query =
  !raw_pkginfo >>= fun raw_pkginfo ->
  if Pkginfo_cache.length pkginfo_tbl = 0 then begin
    Metainfo.fill_metainfo ~add:(Pkginfo_cache.add pkginfo_tbl) raw_pkginfo;
  end;
  Diff.get_pkgs workdir query.Diff.compilers >>= fun pkgs ->
  let html = Diff.get_html ~get_pkginfo query pkgs in
  Html_cache.add html_tbl query html;
  Lwt.return html

let get_html workdir query =
  match Html_cache.find_opt html_tbl query with
  | Some html -> Lwt.return html
  | None -> get_html workdir query
