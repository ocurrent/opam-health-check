open Lwt.Syntax
(* open Server_lib.Intf *)

module Opams_cache = Map.Make (String)
module Revdeps_cache = Map.Make (String)

type merge =
  | Old
  | New

module Pkg_htbl = Map.Make (struct
    type t = string * Server_lib.Intf.Compiler.t
    let compare (full_name, comp) y =
      let full_name = String.compare full_name (fst y) in
      if full_name <> 0 then full_name
      else Server_lib.Intf.Compiler.compare comp (snd y)
  end)

let add_diff htbl acc ((full_name, comp) as pkg) =
  match Pkg_htbl.find pkg htbl with
  | [((Old | New), Server_lib.Intf.State.NotAvailable)] -> acc
  | [(Old, state)] -> Server_lib.Intf.Pkg_diff.{full_name; comp; diff = NotAvailableAnymore state} :: acc
  | [(New, state)] -> Server_lib.Intf.Pkg_diff.{full_name; comp; diff = NowInstallable state} :: acc
  | [(New, new_state); (Old, old_state)] when Server_lib.Intf.State.equal new_state old_state -> acc
  | [(New, new_state); (Old, old_state)] -> Server_lib.Intf.Pkg_diff.{full_name; comp; diff = StatusChanged (old_state, new_state)} :: acc
  | _ -> assert false
  [@@ocaml.warning "-fragile-match"]

let split_diff (bad, partial, not_available, internal_failure, good) diff =
  let open Server_lib.Intf.State in
  let open Server_lib.Intf.Pkg_diff in
  match diff with
  | {diff = (StatusChanged (_, Bad) | NowInstallable Bad); _} -> (diff :: bad, partial, not_available, internal_failure, good)
  | {diff = (StatusChanged (_, Partial) | NowInstallable Partial); _} -> (bad, diff :: partial, not_available, internal_failure, good)
  | {diff = (StatusChanged (_, NotAvailable) | NotAvailableAnymore _); _} -> (bad, partial, diff :: not_available, internal_failure, good)
  | {diff = (StatusChanged (_, InternalFailure) | NowInstallable InternalFailure); _} -> (bad, partial, not_available, diff :: internal_failure, good)
  | {diff = (StatusChanged (_, Good) | NowInstallable Good); _} -> (bad, partial, not_available, internal_failure, diff :: good)
  | {diff = NowInstallable NotAvailable; _} -> assert false

let generate_diff old_pkgs new_pkgs =
  let pkg_htbl = Pkg_htbl.empty in
  let aux pos pkg_htbl pkg =
    Server_lib.Intf.Pkg.instances pkg |>
    List.fold_left begin fun pkg_htbl inst ->
      let comp = Server_lib.Intf.Instance.compiler inst in
      let state = Server_lib.Intf.Instance.state inst in
      let key = (Server_lib.Intf.Pkg.full_name pkg, comp) in
      match Pkg_htbl.find_opt key pkg_htbl with
      | Some acc -> Pkg_htbl.add key ((pos, state) :: acc) pkg_htbl
      | None -> Pkg_htbl.add key [(pos, state)] pkg_htbl
    end pkg_htbl
  in
  let pkg_htbl = List.fold_left (aux Old) pkg_htbl old_pkgs in
  let pkg_htbl = List.fold_left (aux New) pkg_htbl new_pkgs in
  List.sort_uniq ~cmp:Ord.(pair string Server_lib.Intf.Compiler.compare) (List.map fst (Pkg_htbl.bindings pkg_htbl)) |>
  List.fold_left (add_diff pkg_htbl) [] |>
  List.fold_left split_diff ([], [], [], [], [])

type 'a prefetched_or_recompute =
  | Prefetched of 'a
  | Recompute of (unit -> 'a Lwt.t)

type data = {
  mutable logdirs : Server_lib.Server_workdirs.logdir list Lwt.t;
  mutable pkgs : (Server_lib.Server_workdirs.logdir * Server_lib.Intf.Pkg.t list prefetched_or_recompute) list Lwt.t;
  mutable compilers : (Server_lib.Server_workdirs.logdir * Server_lib.Intf.Compiler.t list) list Lwt.t;
  mutable opams : OpamFile.OPAM.t Opams_cache.t Lwt.t;
  mutable revdeps : int Revdeps_cache.t Lwt.t;
}

type t = data Lwt.t ref

let create_data () = {
  logdirs = Lwt.return_nil;
  pkgs = Lwt.return_nil;
  compilers = Lwt.return_nil;
  opams = Lwt.return Opams_cache.empty;
  revdeps = Lwt.return Revdeps_cache.empty;
}

let create () = ref (Lwt.return (create_data ()))

let clear_and_init r_self ~pkgs ~compilers ~logdirs ~opams ~revdeps =
  let timer = Oca_lib.timer_start () in
  let self = create_data () in
  let mvar = Lwt_mvar.create_empty () in
  r_self := begin
    let+ () = Lwt_mvar.take mvar in
    self
  end;
  self.opams <- opams ();
  self.revdeps <- revdeps ();
  self.logdirs <- logdirs ();
  self.compilers <- begin
    let* logdirs = self.logdirs in
    Lwt_list.map_s (fun logdir ->
      let+ c = compilers logdir in
      logdir, c) logdirs
  end;
  self.pkgs <- begin
    let* compilers = self.compilers in
    Lwt_list.mapi_s (fun i (logdir, compilers) ->
      let* p =
        let aux () = pkgs ~compilers logdir in
        match i with
        | 0 | 1 ->
            let+ p = aux () in
            Prefetched p
        | _ ->
            Lwt.return (Recompute aux)
      in
      Lwt.return (logdir, p)
    ) compilers
  end;
  let* _ = self.opams in
  let* _ = self.revdeps in
  let* _ = self.logdirs in
  let* _ = self.compilers in
  let* () = Lwt_mvar.put mvar () in
  let* _ = self.pkgs in
  Oca_lib.timer_log timer Lwt_io.stderr "Cache prefetching"

let is_deprecated flag =
  String.equal (OpamTypesBase.string_of_pkg_flag flag) "deprecated"

let (>>&&) x f =
  let* x = x in
  if x then f () else Lwt.return_false

let must_show_package ~logsearch query ~is_latest pkg =
  let opam = Server_lib.Intf.Pkg.opam pkg in
  let instances' = Server_lib.Intf.Pkg.instances pkg in
  let instances = List.filter (fun inst -> List.mem ~eq:Server_lib.Intf.Compiler.equal (Server_lib.Intf.Instance.compiler inst) query.Html.compilers) instances' in
  begin
    Lwt.return @@
    List.exists (fun comp ->
      match List.find_opt (fun inst -> Server_lib.Intf.Compiler.equal comp (Server_lib.Intf.Instance.compiler inst)) instances' with
      | None -> true (* TODO: Maybe switch to assert false? *)
      | Some inst -> match Server_lib.Intf.Instance.state inst with
        | Server_lib.Intf.State.NotAvailable -> false
        | Server_lib.Intf.State.(Good | Partial | Bad | InternalFailure) -> true
    ) query.Html.show_available
  end >>&& begin fun () ->
    Lwt.return @@
    List.exists (fun state ->
      List.exists (fun inst -> Server_lib.Intf.State.equal state (Server_lib.Intf.Instance.state inst)) instances
    ) query.Html.show_only
  end >>&& begin fun () ->
    Lwt.return @@
    match instances with
    | hd::tl when query.Html.show_diff_only ->
        let state = Server_lib.Intf.Instance.state hd in
        List.exists (fun x -> not (Server_lib.Intf.State.equal state (Server_lib.Intf.Instance.state x))) tl
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
        let+ logsearch = logsearch in
        List.exists (Server_lib.Intf.Pkg.equal pkg) logsearch
    | None -> Lwt.return_true
  end

let filter_pkg ~logsearch query (acc, last) pkg =
  let is_latest = match last with
    | None -> true
    | Some last -> not (String.equal (Server_lib.Intf.Pkg.name pkg) (Server_lib.Intf.Pkg.name last))
  in
  let+ v = must_show_package ~logsearch query ~is_latest pkg in
  match v with
  | true -> pkg :: acc, Some pkg
  | false -> acc, Some pkg

(* TODO: Make use of the cache *)
let get_logsearch ~query ~logdir =
  match query.Html.logsearch with
  | _, None -> Lwt.return []
  | regexp, Some (_, comp) ->
      let switch = Server_lib.Intf.Compiler.to_string comp in
      let+ searches = Server_lib.Server_workdirs.logdir_search ~switch ~regexp logdir in
      searches
      |> List.filter_map (fun s ->
        match String.split_on_char '/' s with
        | [_switch; _state; full_name] -> Some (Server_lib.Intf.Pkg.create ~full_name ~instances:[] ~opam:OpamFile.OPAM.empty ~revdeps:(-1))
        | _ -> None)

let revdeps_cmp p1 p2 =
  Int.neg (Int.compare (Server_lib.Intf.Pkg.revdeps p1) (Server_lib.Intf.Pkg.revdeps p2))

let get_or_recompute = function
  | Prefetched p -> Lwt.return p
  | Recompute f -> f ()

let get_html ~conf self query logdir =
  let aux ~logdir pkgs =
    let* pkgs = pkgs in
    let logsearch = get_logsearch ~query ~logdir in
    let+ (pkgs, _) = Lwt_list.fold_left_s (filter_pkg ~logsearch query) ([], None) (List.rev pkgs) in
    let pkgs = if query.Html.sort_by_revdeps then List.sort revdeps_cmp pkgs else pkgs in
    Html.get_html ~logdir ~conf query pkgs
  in
  let* pkgs = self.pkgs in
  let pkgs = List.assoc ~eq:Server_lib.Server_workdirs.logdir_equal logdir pkgs in
  aux ~logdir (get_or_recompute pkgs)

let get_latest_logdir self =
  let* self = !self in
  let+ v = self.logdirs in
  match v with
  | [] -> None
  | logdir::_ -> Some logdir

let get_html ~conf self query logdir =
  let* self = !self in
  get_html ~conf self query logdir

let get_logdirs self =
  let* self = !self in
  self.logdirs

let get_pkgs ~logdir self =
  let* self = !self in
  let* pkgs = self.pkgs in
  get_or_recompute (List.assoc ~eq:Server_lib.Server_workdirs.logdir_equal logdir pkgs)

let get_compilers ~logdir self =
  let* self = !self in
  let+ compilers = self.compilers in
  List.assoc ~eq:Server_lib.Server_workdirs.logdir_equal logdir compilers

let get_opam self k =
  let* self = !self in
  let+ opams = self.opams in
  Option.get_or ~default:OpamFile.OPAM.empty (Opams_cache.find_opt k opams)

let get_revdeps self k =
  let* self = !self in
  let+ revdeps = self.revdeps in
  Option.get_or ~default:(-1) (Revdeps_cache.find_opt k revdeps)

let get_html_diff ~conf ~old_logdir ~new_logdir self =
  let* old_pkgs = get_pkgs ~logdir:old_logdir self in
  let+ new_pkgs = get_pkgs ~logdir:new_logdir self in
  generate_diff old_pkgs new_pkgs |>
  Html.get_diff ~conf ~old_logdir ~new_logdir

let get_html_diff_list self =
  let* self = !self in
  let+ pkgs = self.pkgs in
  Oca_lib.list_map_cube (fun (new_logdir, _) (old_logdir, _) -> (old_logdir, new_logdir)) pkgs |>
  Html.get_diff_list

let get_html_run_list self =
  let* self = !self in
  let+ pkgs = self.pkgs in
  Html.get_run_list (List.map fst pkgs)

let get_json_latest_packages self =
  let* self = !self in
  let* v = self.logdirs in
  let+ pkgs = match v with
    | [] -> Lwt.return []
    | logdir::_ ->
        let* pkgs = self.pkgs in
        get_or_recompute (List.assoc ~eq:Server_lib.Server_workdirs.logdir_equal logdir pkgs)
  in
  let json = Json.pkgs_to_json pkgs in
  Yojson.Safe.to_string json
