open Lwt.Infix

open Intf

module Html_cache = Hashtbl.Make (struct
    type t = (Server_workdirs.logdir * Html.query)
    let hash = Hashtbl.hash (* TODO: WRONG!! *)
    let equal (logdir1, {Html.available_compilers; compilers; show_available; show_failures_only; show_diff_only; show_latest_only; sort_by_revdeps; maintainers; logsearch}) (logdir2, y) =
      Server_workdirs.logdir_equal logdir1 logdir2 &&
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

module Opams_cache = Hashtbl.Make (String)
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
  [@@ocaml.warning "-fragile-match"]

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
  mutable logdirs : Server_workdirs.logdir list Lwt.t;
  mutable pkgs : (Server_workdirs.logdir * Intf.Pkg.t list Lwt.t) list Lwt.t;
  mutable compilers : (Server_workdirs.logdir * Intf.Compiler.t list) list Lwt.t;
  mutable opams : OpamFile.OPAM.t Opams_cache.t Lwt.t;
  mutable revdeps : int Revdeps_cache.t Lwt.t;
}

let create () = {
  html_tbl = Html_cache.create 32;
  logdirs = Lwt.return_nil;
  pkgs = Lwt.return_nil;
  compilers = Lwt.return_nil;
  opams = Lwt.return (Opams_cache.create 0);
  revdeps = Lwt.return (Revdeps_cache.create 0);
}

let clear_and_init self ~pkgs ~compilers ~logdirs ~opams ~revdeps =
  self.opams <- opams ();
  self.revdeps <- revdeps ();
  self.logdirs <- logdirs ();
  self.compilers <- self.logdirs >>= Lwt_list.map_s (fun logdir -> compilers logdir >|= fun c -> (logdir, c));
  self.pkgs <- self.compilers >|= List.map (fun (logdir, compilers) -> (logdir, pkgs ~compilers logdir));
  Html_cache.clear self.html_tbl

let is_deprecated flag =
  String.equal (OpamTypesBase.string_of_pkg_flag flag) "deprecated"

let (>>&&) x f =
  x >>= fun x ->
  if x then f () else Lwt.return_false

let must_show_package ~logsearch query ~is_latest pkg =
  let opam = Pkg.opam pkg in
  let instances' = Pkg.instances pkg in
  let instances = List.filter (fun inst -> List.mem ~eq:Compiler.equal (Instance.compiler inst) query.Html.compilers) instances' in
  begin
    Lwt.return @@
    List.exists (fun comp ->
      match List.find_opt (fun inst -> Compiler.equal comp (Instance.compiler inst)) instances' with
      | None -> true (* TODO: Maybe switch to assert false? *)
      | Some inst -> match Instance.state inst with
        | State.NotAvailable -> false
        | State.(Good | Partial | Bad | InternalFailure) -> true
    ) query.Html.show_available
  end >>&& begin fun () ->
    Lwt.return @@
    if query.Html.show_failures_only then
      List.exists (fun instance -> match Instance.state instance with
        | State.Bad | State.Partial -> true
        | State.Good | State.NotAvailable | State.InternalFailure -> false
      ) instances
    else
      true
  end >>&& begin fun () ->
    Lwt.return @@
    match instances with
    | hd::tl when query.Html.show_diff_only ->
        let state = Instance.state hd in
        List.exists (fun x -> not (State.equal state (Instance.state x))) tl
    | [] | _::_ ->
        true
  end >>&& begin fun () ->
    Lwt.return @@
    if query.Html.show_latest_only then
      if is_latest then
        not (List.exists is_deprecated opam.OpamFile.OPAM.flags)
      else
        false
    else
      true
  end >>&& begin fun () ->
    Lwt.return @@
    match snd query.Html.maintainers with
    | Some re -> List.exists (Re.execp re) opam.OpamFile.OPAM.maintainer
    | None -> true
  end >>&& begin fun () ->
    match snd query.Html.logsearch with
    | Some _ -> logsearch >|= List.exists (Pkg.equal pkg)
    | None -> Lwt.return_true
  end

let filter_pkg ~logsearch query (acc, last) pkg =
  let is_latest = match last with
    | None -> true
    | Some last -> not (String.equal (Pkg.name pkg) (Pkg.name last))
  in
  must_show_package ~logsearch query ~is_latest pkg >|= function
  | true -> (pkg :: acc, Some pkg)
  | false -> (acc, Some pkg)

(* TODO: Make use of the cache *)
let get_logsearch ~query ~logdir =
  match query.Html.logsearch with
  | _, None -> Lwt.return []
  | regexp, Some (_, comp) ->
      let switch = Compiler.to_string comp in
      Server_workdirs.logdir_search ~switch ~regexp logdir >|=
      List.filter_map (fun s ->
        match String.split_on_char '/' s with
        | [_switch; _state; full_name] -> Some (Pkg.create ~full_name ~instances:[] ~opam:OpamFile.OPAM.empty ~revdeps:(-1))
        | _ -> None
      )

let revdeps_cmp p1 p2 =
  Int.neg (Int.compare (Intf.Pkg.revdeps p1) (Intf.Pkg.revdeps p2))

let get_html self query logdir =
  let aux ~logdir pkgs =
    pkgs >>= fun pkgs ->
    let logsearch = get_logsearch ~query ~logdir in
    Lwt_list.fold_left_s (filter_pkg ~logsearch query) ([], None) (List.rev pkgs) >>= fun (pkgs, _) ->
    let pkgs = if query.Html.sort_by_revdeps then List.sort revdeps_cmp pkgs else pkgs in
    let html = Html.get_html ~logdir query pkgs in
    Html_cache.add self.html_tbl (logdir, query) html;
    Lwt.return html
  in
  self.pkgs >>= fun pkgs ->
  let pkgs = List.assoc ~eq:Server_workdirs.logdir_equal logdir pkgs in
  aux ~logdir pkgs

let get_latest_logdir self =
  self.logdirs >>= function
  | [] -> Lwt.fail Not_found
  | logdir::_ -> Lwt.return logdir

let get_html self query logdir =
  match Html_cache.find_opt self.html_tbl (logdir, query) with
  | Some html -> Lwt.return html
  | None -> get_html self query logdir

let get_logdirs self =
  self.logdirs

let get_pkgs ~logdir self =
  self.pkgs >>= List.assoc ~eq:Server_workdirs.logdir_equal logdir

let get_compilers ~logdir self =
  self.compilers >|= List.assoc ~eq:Server_workdirs.logdir_equal logdir

let get_opam self k =
  self.opams >|= fun opams ->
  Option.get_or ~default:OpamFile.OPAM.empty (Opams_cache.find_opt opams k)

let get_revdeps self k =
  self.revdeps >|= fun revdeps ->
  Option.get_or ~default:(-1) (Revdeps_cache.find_opt revdeps k)

let get_html_diff ~old_logdir ~new_logdir self =
  get_pkgs ~logdir:old_logdir self >>= fun old_pkgs ->
  get_pkgs ~logdir:new_logdir self >|= fun new_pkgs ->
  generate_diff old_pkgs new_pkgs |>
  Html.get_diff ~old_logdir ~new_logdir

let get_html_diff_list self =
  self.pkgs >|= fun pkgs ->
  Oca_lib.list_map_cube (fun (new_logdir, _) (old_logdir, _) -> (old_logdir, new_logdir)) pkgs |>
  Html.get_diff_list

let get_html_run_list self =
  self.pkgs >|= fun pkgs ->
  Html.get_run_list (List.map fst pkgs)
