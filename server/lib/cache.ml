open Intf

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

type 'a prefetched_or_recompute =
  | Prefetched of 'a
  | Recompute of (unit -> 'a)

type data = {
  mutable logdirs : Server_workdirs.logdir list Lwt.t;
  mutable pkgs : (Server_workdirs.logdir * Intf.Pkg.t list Lwt.t prefetched_or_recompute) list Lwt.t;
  mutable compilers : (Server_workdirs.logdir * Intf.Compiler.t list Lwt.t Lazy.t) list Lwt.t;
  mutable opams : OpamFile.OPAM.t Opams_cache.t Lwt.t;
  mutable revdeps : int Revdeps_cache.t Lwt.t;
}

type t = data Lwt.t ref

let create_data () = {
  logdirs = Lwt.return_nil;
  pkgs = Lwt.return_nil;
  compilers = Lwt.return_nil;
  opams = Lwt.return (Opams_cache.create 0);
  revdeps = Lwt.return (Revdeps_cache.create 0);
}

let create () = ref (Lwt.return (create_data ()))

let clear_and_init r_self ~pkgs ~compilers ~logdirs ~opams ~revdeps =
  let timer = Oca_lib.timer_start () in
  let self = create_data () in
  let mvar = Lwt_mvar.create_empty () in
  r_self := begin
    let%lwt () = Lwt_mvar.take mvar in
    Lwt.return self
  end;
  self.opams <- opams ();
  self.revdeps <- revdeps ();
  self.logdirs <- logdirs ();
  self.compilers <- begin
    let%lwt logdirs = self.logdirs in
    List.map (fun logdir ->
      let c = lazy (compilers logdir) in
      (logdir, c)
    ) logdirs |>
    Lwt.return
  end;
  self.pkgs <- begin
    let%lwt compilers = self.compilers in
    List.mapi (fun i (logdir, compilers) ->
      let p =
        let aux () =
          let%lwt compilers = Lazy.force compilers in
          pkgs ~compilers logdir
        in
        match i with
        | 0 | 1 -> Prefetched (aux ())
        | _ -> Recompute aux
      in
      (logdir, p)
    ) compilers |>
    Lwt.return
  end;
  let%lwt _ = self.opams in
  let%lwt _ = self.revdeps in
  let%lwt _ = self.logdirs in
  let%lwt _ = self.compilers in
  let%lwt () = Lwt_mvar.put mvar () in
  let%lwt () =
    let%lwt pkgs = self.pkgs in
    Lwt_list.iter_s (function
      | _, Prefetched p ->
          let%lwt _ = p in
          Lwt.return_unit
      | _, Recompute _ -> Lwt.return_unit
    ) pkgs
  in
  Oca_lib.timer_log timer Lwt_io.stderr "Cache prefetching"

let is_deprecated flag =
  String.equal (OpamTypesBase.string_of_pkg_flag flag) "deprecated"

let (>>&&) x f =
  let%lwt x = x in
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
    List.exists (fun state ->
      List.exists (fun inst -> State.equal state (Instance.state inst)) instances
    ) query.Html.show_only
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
    | Some _ ->
        let%lwt logsearch = logsearch in
        Lwt.return (List.exists (Pkg.equal pkg) logsearch)
    | None -> Lwt.return_true
  end

let filter_pkg ~logsearch query (acc, last) pkg =
  let is_latest = match last with
    | None -> true
    | Some last -> not (String.equal (Pkg.name pkg) (Pkg.name last))
  in
  match%lwt must_show_package ~logsearch query ~is_latest pkg with
  | true -> Lwt.return (pkg :: acc, Some pkg)
  | false -> Lwt.return (acc, Some pkg)

(* TODO: Make use of the cache *)
let get_logsearch ~query ~logdir =
  match query.Html.logsearch with
  | _, None -> Lwt.return []
  | regexp, Some (_, comp) ->
      let switch = Compiler.to_string comp in
      let%lwt searches = Server_workdirs.logdir_search ~switch ~regexp logdir in
      List.filter_map (fun s ->
        match String.split_on_char '/' s with
        | [_switch; _state; full_name] -> Some (Pkg.create ~full_name ~instances:[] ~opam:OpamFile.OPAM.empty ~revdeps:(-1))
        | _ -> None
      ) searches |>
      Lwt.return

let revdeps_cmp p1 p2 =
  Int.neg (Int.compare (Intf.Pkg.revdeps p1) (Intf.Pkg.revdeps p2))

let get_or_recompute = function
  | Prefetched p -> p
  | Recompute f -> f ()

let get_html ~conf self query logdir =
  let aux ~logdir pkgs =
    let%lwt pkgs = pkgs in
    let logsearch = get_logsearch ~query ~logdir in
    let%lwt (pkgs, _) = Lwt_list.fold_left_s (filter_pkg ~logsearch query) ([], None) (List.rev pkgs) in
    let pkgs = if query.Html.sort_by_revdeps then List.sort revdeps_cmp pkgs else pkgs in
    let html = Html.get_html ~logdir ~conf query pkgs in
    Lwt.return html
  in
  let%lwt pkgs = self.pkgs in
  let pkgs = List.assoc ~eq:Server_workdirs.logdir_equal logdir pkgs in
  aux ~logdir (get_or_recompute pkgs)

let get_latest_logdir self =
  let%lwt self = !self in
  match%lwt self.logdirs with
  | [] -> Lwt.return None
  | logdir::_ -> Lwt.return (Some logdir)

let get_html ~conf self query logdir =
  let%lwt self = !self in
  get_html ~conf self query logdir

let get_logdirs self =
  let%lwt self = !self in
  self.logdirs

let get_pkgs ~logdir self =
  let%lwt self = !self in
  let%lwt pkgs = self.pkgs in
  get_or_recompute (List.assoc ~eq:Server_workdirs.logdir_equal logdir pkgs)

let get_compilers ~logdir self =
  let%lwt self = !self in
  let%lwt compilers = self.compilers in
  Lazy.force (List.assoc ~eq:Server_workdirs.logdir_equal logdir compilers)

let get_opam self k =
  let%lwt self = !self in
  let%lwt opams = self.opams in
  Lwt.return (Option.get_or ~default:OpamFile.OPAM.empty (Opams_cache.find_opt opams k))

let get_revdeps self k =
  let%lwt self = !self in
  let%lwt revdeps = self.revdeps in
  Lwt.return (Option.get_or ~default:(-1) (Revdeps_cache.find_opt revdeps k))

let get_html_diff ~conf ~old_logdir ~new_logdir self =
  let%lwt old_pkgs = get_pkgs ~logdir:old_logdir self in
  let%lwt new_pkgs = get_pkgs ~logdir:new_logdir self in
  generate_diff old_pkgs new_pkgs |>
  Html.get_diff ~conf ~old_logdir ~new_logdir |>
  Lwt.return

let get_html_diff_list self =
  let%lwt self = !self in
  let%lwt pkgs = self.pkgs in
  Oca_lib.list_map_cube (fun (new_logdir, _) (old_logdir, _) -> (old_logdir, new_logdir)) pkgs |>
  Html.get_diff_list |>
  Lwt.return

let get_html_run_list self =
  let%lwt self = !self in
  let%lwt pkgs = self.pkgs in
  Lwt.return (Html.get_run_list (List.map fst pkgs))

let get_json_latest_packages self =
  let%lwt self = !self in
  let%lwt json =
    let%lwt pkgs = match%lwt self.logdirs with
      | [] -> Lwt.return []
      | logdir::_ ->
          let%lwt pkgs = self.pkgs in
          get_or_recompute (List.assoc ~eq:Server_workdirs.logdir_equal logdir pkgs)
    in
    let json = Json.pkgs_to_json pkgs in
    Lwt.return (Yojson.Safe.to_string json)
  in
  Lwt.return json
