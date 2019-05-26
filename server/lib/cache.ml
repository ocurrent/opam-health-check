open Lwt.Infix

open Intf

module Html_cache = Hashtbl.Make (struct
    type t = Html.query
    let hash = Hashtbl.hash (* TODO: WRONG!! *)
    let equal {Html.available_compilers; compilers; show_available; show_failures_only; show_diff_only; show_latest_only; sort_by_revdeps; maintainers; logsearch} y =
      List.equal Compiler.equal available_compilers y.Html.available_compilers &&
      List.equal Compiler.equal compilers y.Html.compilers &&
      List.equal Compiler.equal show_available y.Html.show_available &&
      Bool.equal show_failures_only y.Html.show_failures_only &&
      Bool.equal show_diff_only y.Html.show_diff_only &&
      Bool.equal show_latest_only y.Html.show_latest_only &&
      Bool.equal sort_by_revdeps y.Html.sort_by_revdeps &&
      String.equal (fst maintainers) (fst y.Html.maintainers) &&
      String.equal (fst logsearch) (fst y.Html.logsearch) &&
      Option.equal (fun (_, comp1) (_, comp2) -> Intf.Compiler.equal comp1 comp2) (snd logsearch) (snd y.Html.logsearch)
  end)

module Maintainers_cache = Hashtbl.Make (String)
module Revdeps_cache = Hashtbl.Make (String)

type merge =
  | Old
  | New

module Pkg_htbl = CCHashtbl.Make (struct
    type t = string * Compiler.t
    let hash = Hashtbl.hash (* TODO: Improve *)
    let equal (full_name, comp) y =
      String.equal full_name (fst y) &&
      Intf.Compiler.equal comp (snd y)
  end)

let add_diff htbl acc ((full_name, comp) as pkg) =
  match Pkg_htbl.find_all htbl pkg with
  | [((Old | New), Intf.State.NotAvailable)] -> acc
  | [(Old, state)] -> Intf.Pkg_diff.{full_name; comp; diff = NotAvailableAnymore state} :: acc
  | [(New, state)] -> Intf.Pkg_diff.{full_name; comp; diff = NowInstallable state} :: acc
  | [(New, new_state); (Old, old_state)] when Intf.State.equal new_state old_state -> acc
  | [(New, new_state); (Old, old_state)] -> Intf.Pkg_diff.{full_name; comp; diff = StatusChanged (old_state, new_state)} :: acc
  | _ -> assert false

let split_diff (bad, partial, not_available, internal_failure, good) diff =
  let open Intf.State in
  let open Intf.Pkg_diff in
  match diff with
  | {diff = (StatusChanged (_, Bad) | NowInstallable Bad); _} -> (diff :: bad, partial, not_available, internal_failure, good)
  | {diff = (StatusChanged (_, Partial) | NowInstallable Partial); _} -> (bad, diff :: partial, not_available, internal_failure, good)
  | {diff = (StatusChanged (_, NotAvailable) | NotAvailableAnymore _); _} -> (bad, partial, diff :: not_available, internal_failure, good)
  | {diff = (StatusChanged (_, InternalFailure) | NowInstallable InternalFailure); _} -> (bad, partial, not_available, diff :: internal_failure, good)
  | {diff = (StatusChanged (_, Good) | NowInstallable Good); _} -> (bad, partial, not_available, internal_failure, diff :: good)
  | {diff = NowInstallable NotAvailable; _} -> assert false

let generate_diff old_pkgs new_pkgs =
  let pkg_htbl = Pkg_htbl.create 10_000 in
  let aux pos pkg =
    Intf.Pkg.instances pkg |>
    List.iter begin fun inst ->
      let comp = Intf.Instance.compiler inst in
      let state = Intf.Instance.state inst in
      Pkg_htbl.add pkg_htbl (Intf.Pkg.full_name pkg, comp) (pos, state)
    end
  in
  List.iter (aux Old) old_pkgs;
  List.iter (aux New) new_pkgs;
  List.sort_uniq ~cmp:Ord.(pair string Intf.Compiler.compare) (Pkg_htbl.keys_list pkg_htbl) |>
  List.fold_left (add_diff pkg_htbl) [] |>
  List.fold_left split_diff ([], [], [], [], [])

type t = {
  html_tbl : string Html_cache.t;
  mutable pkgs : (Server_workdirs.logdir * Intf.Pkg.t list Lwt.t) list Lwt.t;
  mutable compilers : (Server_workdirs.logdir * Intf.Compiler.t list Lwt.t) list Lwt.t;
  mutable maintainers : string list Maintainers_cache.t Lwt.t;
  mutable revdeps : int Revdeps_cache.t Lwt.t;
  mutable pkgs_diff : (Pkg_diff.t list * Pkg_diff.t list * Pkg_diff.t list * Intf.Pkg_diff.t list * Intf.Pkg_diff.t list) Lwt.t;
  mutable html_diff : string Lwt.t;
}

let create () = {
  html_tbl = Html_cache.create 32;
  pkgs = Lwt.return_nil;
  compilers = Lwt.return_nil;
  maintainers = Lwt.return (Maintainers_cache.create 0);
  revdeps = Lwt.return (Revdeps_cache.create 0);
  pkgs_diff = Lwt.return ([], [], [], [], []);
  html_diff = Lwt.return "";
}

let call_pkgs ~pkgs = function
  | [] ->
      Lwt.return_nil
  | logdir::logdirs ->
      let pkg = pkgs ~old:false logdir in
      let pkgs = List.map (fun logdir -> (logdir, pkgs ~old:true logdir)) logdirs in
      Lwt.return ((logdir, pkg) :: pkgs)

let call_generate_diff = function
  | (_, new_pkgs)::(_, old_pkgs)::_ -> new_pkgs >>= fun new_pkgs -> old_pkgs >|= fun old_pkgs -> generate_diff old_pkgs new_pkgs
  | (_, new_pkgs)::_ -> new_pkgs >|= generate_diff []
  | [] -> Lwt.return (generate_diff [] [])

let call_html_diff ~html_diff = function
  | new_logdir::old_logdir::_ -> html_diff ~old_logdir:(Some old_logdir) ~new_logdir:(Some new_logdir)
  | new_logdir::_ -> html_diff ~old_logdir:None ~new_logdir:(Some new_logdir)
  | [] -> html_diff ~old_logdir:None ~new_logdir:None

let clear_and_init self ~pkgs ~compilers ~logdirs ~maintainers ~revdeps ~html_diff =
  self.maintainers <- maintainers ();
  self.revdeps <- revdeps ();
  let logdirs = logdirs () in
  self.compilers <- logdirs >|= List.map (fun logdir -> (logdir, compilers logdir));
  self.pkgs <- logdirs >>= call_pkgs ~pkgs;
  self.pkgs_diff <- self.pkgs >>= call_generate_diff;
  self.html_diff <- logdirs >>= call_html_diff ~html_diff;
  Html_cache.clear self.html_tbl

let get_html self query =
  self.pkgs >>= fun pkgs ->
  let (logdir, pkgs) = Option.map_or ~default:(None, Lwt.return_nil) (fun (logdir, pkgs) -> (Some logdir, pkgs)) (List.head_opt pkgs) in
  pkgs >>= fun pkgs ->
  Html.get_html logdir query pkgs >>= fun html ->
  Html_cache.add self.html_tbl query html;
  Lwt.return html

let get_html self query =
  match Html_cache.find_opt self.html_tbl query with
  | Some html -> Lwt.return html
  | None -> get_html self query

let get_pkgs ~logdir self =
  self.pkgs >>= List.assoc ~eq:Server_workdirs.logdir_equal logdir

let get_compilers ~old self =
  self.compilers >>= function
  | (_, compilers)::_ when not old -> compilers
  | _::(_, compilers)::_ when old -> compilers
  | _ -> Lwt.return_nil

let get_maintainers self k =
  self.maintainers >|= fun maintainers ->
  Option.get_or ~default:[] (Maintainers_cache.find_opt maintainers k)

let get_revdeps self k =
  self.revdeps >|= fun revdeps ->
  Option.get_or ~default:(-1) (Revdeps_cache.find_opt revdeps k)

let get_diff self =
  self.pkgs_diff

let get_html_diff self =
  self.html_diff
