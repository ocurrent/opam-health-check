open Lwt.Infix

module Pkgs = Map.Make (String)

type state = Good | Partial | Bad

type comp = string
type pkg = string
type pkgs = (pkg * (comp * state) list) list

type query = {
  compilers : comp list;
  show_available : comp list;
  show_failures_only : bool;
  show_diff_only : bool;
  show_latest_only : bool;
  maintainers : string * Re.re;
}

let state_eq x y = match x, y with
  | Good, Good | Partial, Partial | Bad, Bad -> true
  | Good, _ | Partial, _ | Bad, _ -> false

let get_files dirname =
  Lwt_unix.opendir (Fpath.to_string dirname) >>= fun dir ->
  let rec aux files =
    Lwt.catch begin fun () ->
      Lwt_unix.readdir dir >>= fun file ->
      if Fpath.is_rel_seg file then
        aux files
      else
        aux (file :: files)
    end begin function
    | End_of_file -> Lwt.return files
    | exn -> Lwt.fail exn
    end
  in
  aux [] >>= fun files ->
  Lwt_unix.closedir dir >|= fun () ->
  files

let is_directory dir file =
  Sys.is_directory (Fpath.to_string (Fpath.add_seg dir file))

let get_dirs dir =
  get_files dir >|= fun files ->
  let dirs = List.filter (is_directory dir) files in
  List.sort OpamVersionCompare.compare dirs

let default_query workdir =
  let logdir = Server_workdirs.logdir workdir in
  get_dirs logdir >|= fun compilers ->
  { compilers;
    show_available = compilers;
    show_failures_only = false;
    show_diff_only = false;
    show_latest_only = false;
    maintainers = ("", Re.Posix.compile_pat "");
  }

let pkg_update ~comp v pkgs pkg =
  let aux = function
    | None -> Some [(comp, v)]
    | Some l -> Some ((comp, v) :: l)
  in
  Pkgs.update pkg aux pkgs

let get_pkgs_from_dir workdir pkgs comp =
  get_files (Server_workdirs.gooddir ~switch:comp workdir) >>= fun good_files ->
  get_files (Server_workdirs.partialdir ~switch:comp workdir) >>= fun partial_files ->
  get_files (Server_workdirs.baddir ~switch:comp workdir) >|= fun bad_files ->
  let pkgs = List.fold_left (pkg_update ~comp Good) pkgs good_files in
  let pkgs = List.fold_left (pkg_update ~comp Partial) pkgs partial_files in
  List.fold_left (pkg_update ~comp Bad) pkgs bad_files

let get_pkgs workdir compilers =
  Lwt_list.fold_left_s (get_pkgs_from_dir workdir) Pkgs.empty compilers >|= fun pkgs ->
  let pkgs = Pkgs.bindings pkgs in
  List.sort (fun (x, _) (y, _) -> OpamVersionCompare.compare x y) pkgs

let instance_to_html ~pkg instances comp =
  let open Tyxml.Html in
  let td c = td ~a:[a_class ["result-col"; "results-cell"]; a_style ("background-color: "^c^";")] in
  match List.Assoc.get ~eq:String.equal comp instances with
  | Some Good -> td "green" [a ~a:[a_href ("/"^comp^"/good/"^pkg)] [pcdata "☑"]]
  | Some Partial -> td "orange" [a ~a:[a_href ("/"^comp^"/partial/"^pkg)] [pcdata "☒"]]
  | Some Bad -> td "red" [a ~a:[a_href ("/"^comp^"/bad/"^pkg)] [pcdata "☒"]]
  | None -> td "grey" [pcdata "☐"]

let get_pkg_name pkg =
  match String.index_opt pkg '.' with
  | Some idx -> String.sub pkg 0 idx
  | None -> pkg (* TODO: Should raise an exception or a warning somewhere *)

let must_show_package query ~get_pkginfo ~last ~pkg instances =
  List.exists (fun comp -> List.Assoc.mem ~eq:String.equal comp instances) query.show_available &&
  begin
    if query.show_failures_only then
      List.exists (function (_, (Bad | Partial)) -> true | (_, Good) -> false) instances
    else
      true
  end &&
  begin
    if query.show_diff_only && not (List.is_empty instances) then
      let state = snd (List.hd instances) in
      List.exists (fun (_, x) -> not (state_eq state x)) (List.tl instances)
    else
      true
  end &&
  begin
    if query.show_latest_only then
      match last with
      | None -> true
      | Some last -> not (String.equal (get_pkg_name pkg) (get_pkg_name last))
    else
      true
  end &&
  begin
    if not (String.is_empty (fst query.maintainers)) then
      List.exists (Re.execp (snd query.maintainers)) (get_pkginfo (get_pkg_name pkg)).Metainfo.maintainers
    else
      true
  end

let pkg_to_html ~get_pkginfo query (acc, last) (pkg, instances) =
  let open Tyxml.Html in
  let tr = tr ~a:[a_class ["results-row"]] in
  let td = td ~a:[a_class ["results-cell"; "pkgname"]] in
  if must_show_package query ~get_pkginfo ~last ~pkg instances then
    ((tr (td [pcdata pkg] :: List.map (instance_to_html ~pkg instances) query.compilers)) :: acc, Some pkg)
  else
    (acc, Some pkg)

let gen_table_form l =
  let open Tyxml.Html in
  let aux (txt, elt) = tr [td txt; td [elt]] in
  let legend = legend [b [pcdata "Filter form:"]] in
  form [fieldset ~legend [table (List.map aux l)]]

let get_html ~get_pkginfo query pkgs =
  let open Tyxml.Html in
  let col_width = string_of_int (100 / max 1 (List.length query.compilers)) in
  let pkgs, _ = List.fold_left (pkg_to_html ~get_pkginfo query) ([], None) (List.rev pkgs) in
  let th ?(a=[]) = th ~a:(a_class ["results-cell"]::a) in
  let dirs = th [] :: List.map (fun comp -> th ~a:[a_class ["result-col"]] [pcdata comp]) query.compilers in
  let title = title (pcdata "opam-check-all") in
  let charset = meta ~a:[a_charset "utf-8"] () in
  let style_table = pcdata ".results {border: 2px solid black; border-collapse: collapse; min-width: 100%;}" in
  let style_col = pcdata (".result-col {text-align: center; width: "^col_width^"%;}") in
  let style_case = pcdata ".results-cell {border-left: 2px solid black;}" in
  let style_row = pcdata ".results-row {border-top: 2px solid black; border-bottom: 2px solid black;}" in
  let style_row_hover = pcdata ".results-row:hover {border-top-width: 4px; border-bottom-width: 4px;}" in
  let style_pkgname = pcdata ".pkgname {white-space: nowrap;}" in
  let head = head title [charset; style [style_table; style_col; style_case; style_pkgname; style_row; style_row_hover]] in
  let compilers_text = [pcdata "Show only [list of compilers separated by ':']:"] in
  let compilers = input ~a:[a_input_type `Text; a_name "compilers"; a_value (String.concat ":" query.compilers)] () in
  let show_available_text = [pcdata "Show only packages available in [list of compilers separated by ':']:"] in
  let show_available = input ~a:[a_input_type `Text; a_name "show-available"; a_value (String.concat ":" query.show_available)] () in
  let show_failures_only_text = [pcdata "Show failures only:"] in
  let show_failures_only = input ~a:(a_input_type `Checkbox::a_name "show-failures-only"::a_value "true"::if query.show_failures_only then [a_checked ()] else []) () in
  let show_diff_only_text = [pcdata "Only show packages that have different build status between each compilers:"] in
  let show_diff_only = input ~a:(a_input_type `Checkbox::a_name "show-diff-only"::a_value "true"::if query.show_diff_only then [a_checked ()] else []) () in
  let show_latest_only_text = [pcdata "Only show the latest version of each packages:"] in
  let show_latest_only = input ~a:(a_input_type `Checkbox::a_name "show-latest-only"::a_value "true"::if query.show_latest_only then [a_checked ()] else []) () in
  let maintainers_text = [pcdata "Show only packages maintained by [posix regexp]:"] in
  let maintainers = input ~a:[a_input_type `Text; a_name "maintainers"; a_value (fst query.maintainers)] () in
  let submit_form = input ~a:[a_input_type `Submit; a_value "Submit"] () in
  let filter_form = gen_table_form [
    (compilers_text, compilers);
    (show_available_text, show_available);
    (show_failures_only_text, show_failures_only);
    (show_diff_only_text, show_diff_only);
    (show_latest_only_text, show_latest_only);
    (maintainers_text, maintainers);
    ([], submit_form);
  ] in
  let doc = table ~a:[a_class ["results"]] ~thead:(thead [tr dirs]) pkgs in
  let doc = html head (body [filter_form; br (); doc]) in
  Format.sprintf "%a\n" (pp ()) doc

let comp_from_string x = x
let comp_equal = String.equal
