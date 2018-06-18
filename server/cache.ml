open Lwt.Infix

module Pkg = Backend.Pkg

module Html_cache = Hashtbl.Make (struct
    type t = Diff.query
    let hash = Hashtbl.hash
    let equal {Diff.compilers; show_available; show_failures_only; show_diff_only; show_latest_only; maintainers} y =
      List.equal Pkg.comp_equal compilers y.Diff.compilers &&
      List.equal Pkg.comp_equal show_available y.Diff.show_available &&
      Bool.equal show_failures_only y.Diff.show_failures_only &&
      Bool.equal show_diff_only y.Diff.show_diff_only &&
      Bool.equal show_latest_only y.Diff.show_latest_only &&
      String.equal (fst maintainers) (fst y.Diff.maintainers)
  end)
module Pkginfo_cache = Hashtbl.Make (struct
    type t = Pkg.pkg
    let hash = Hashtbl.hash
    let equal = Pkg.pkg_equal
  end)

let pkgsinfo = ref Lwt.return_nil
let html_tbl = Html_cache.create 32
let pkgs = ref Lwt.return_nil

let update_pkg pkginfo_tbl pkgsinfo pkg f =
  let info =
    match Pkginfo_cache.find_opt pkginfo_tbl pkg with
    | Some info -> {info with Pkg.instances = f info.Pkg.instances}
    | None ->
        let instances = f [] in
        let pkg = Pkg.pkg_name_to_string pkg in
        begin match List.find_opt (fun pkg' -> String.equal pkg'.Obi.Index.name pkg) pkgsinfo with
        | Some pkginfo -> {Pkg.maintainers = pkginfo.Obi.Index.maintainers; instances}
        | None -> {Pkg.maintainers = []; instances}
        end
  in
  Pkginfo_cache.replace pkginfo_tbl pkg info

let update_pkgs backend =
  !pkgsinfo >>= fun pkgsinfo ->
  let pkginfo_tbl = Pkginfo_cache.create 10_000 in
  let update = update_pkg pkginfo_tbl pkgsinfo in
  Pkg.fill_pkgs ~update backend >|= fun () ->
  let pkgs = Pkginfo_cache.fold (fun pkg info acc -> (pkg, info)::acc) pkginfo_tbl [] in
  List.sort (fun (x, _) (y, _) -> Pkg.pkg_compare x y) pkgs

let clear_and_init backend =
  pkgsinfo := Metainfo.get_pkgsinfo ();
  pkgs := update_pkgs backend;
  Html_cache.clear html_tbl

let get_html query =
  !pkgs >>= fun pkgs ->
  let html = Diff.get_html query pkgs in
  Html_cache.add html_tbl query html;
  Lwt.return html

let get_html query =
  match Html_cache.find_opt html_tbl query with
  | Some html -> Lwt.return html
  | None -> get_html query
