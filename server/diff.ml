module Pkg = Backend.Pkg

type query = {
  compilers : Pkg.comp list;
  show_available : Pkg.comp list;
  show_failures_only : bool;
  show_diff_only : bool;
  show_latest_only : bool;
  maintainers : string * Re.re;
}

let instance_to_html ~pkg instances comp =
  let open Tyxml.Html in
  let td c = td ~a:[a_class ["result-col"; "results-cell"]; a_style ("background-color: "^c^";")] in
  match List.Assoc.get ~eq:Pkg.comp_equal comp instances with
  | Some Pkg.Good -> td "green" [a ~a:[a_href ("/"^Pkg.comp_to_string comp^"/good/"^Pkg.pkg_to_string pkg)] [pcdata "☑"]]
  | Some Pkg.Partial -> td "orange" [a ~a:[a_href ("/"^Pkg.comp_to_string comp^"/partial/"^Pkg.pkg_to_string pkg)] [pcdata "☒"]]
  | Some Pkg.Bad -> td "red" [a ~a:[a_href ("/"^Pkg.comp_to_string comp^"/bad/"^Pkg.pkg_to_string pkg)] [pcdata "☒"]]
  | None -> td "grey" [pcdata "☐"]

let must_show_package query ~last ~pkg {Pkg.maintainers; instances} =
  List.exists (fun comp -> List.Assoc.mem ~eq:Pkg.comp_equal comp instances) query.show_available &&
  begin
    if query.show_failures_only then
      List.exists (function (_, (Pkg.Bad | Pkg.Partial)) -> true | (_, Pkg.Good) -> false) instances
    else
      true
  end &&
  begin
    if query.show_diff_only && not (List.is_empty instances) then
      let state = snd (List.hd instances) in
      List.exists (fun (_, x) -> not (Pkg.state_eq state x)) (List.tl instances)
    else
      true
  end &&
  begin
    if query.show_latest_only then
      match last with
      | None -> true
      | Some last -> not (String.equal (Pkg.pkg_name_to_string pkg) (Pkg.pkg_name_to_string last))
    else
      true
  end &&
  begin
    if not (String.is_empty (fst query.maintainers)) then
      List.exists (Re.execp (snd query.maintainers)) maintainers
    else
      true
  end

let pkg_to_html query (acc, last) (pkg, info) =
  let open Tyxml.Html in
  let tr = tr ~a:[a_class ["results-row"]] in
  let td = td ~a:[a_class ["results-cell"; "pkgname"]] in
  if must_show_package query ~last ~pkg info then
    ((tr (td [pcdata (Pkg.pkg_to_string pkg)] :: List.map (instance_to_html ~pkg info.Pkg.instances) query.compilers)) :: acc, Some pkg)
  else
    (acc, last)

let result_legend =
  let open Tyxml.Html in
  let legend = legend [b [pcdata "Legend:"]] in
  fieldset ~legend [table ~a:[a_style "white-space: nowrap;"] [
    tr [td ~a:[a_style "background-color: green;"] [pcdata "☑"]; td [pcdata "Package successfully built"]];
    tr [td ~a:[a_style "background-color: orange;"] [pcdata "☒"]; td [pcdata "One of the dependencies failed to build"]];
    tr [td ~a:[a_style "background-color: red;"] [pcdata "☒"]; td [pcdata "Package failed to build"]];
    tr [td ~a:[a_style "background-color: grey;"] [pcdata "☐"]; td [pcdata "Package is not available in this environment"]];
  ]]

let gen_table_form l =
  let open Tyxml.Html in
  let aux (txt, elt) = tr [td txt; td [elt]] in
  let legend = legend [b [pcdata "Filter form:"]] in
  form [fieldset ~legend [table [tr [td ~a:[a_style "width: 100%;"] [table (List.map aux l)]; td [result_legend]]]]]

let get_html query pkgs =
  let open Tyxml.Html in
  let col_width = string_of_int (100 / max 1 (List.length query.compilers)) in
  let pkgs, _ = List.fold_left (pkg_to_html query) ([], None) (List.rev pkgs) in
  let th ?(a=[]) = th ~a:(a_class ["results-cell"]::a) in
  let dirs = th [] :: List.map (fun comp -> th ~a:[a_class ["result-col"]] [pcdata (Pkg.comp_to_string comp)]) query.compilers in
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
  let compilers = input ~a:[a_input_type `Text; a_name "compilers"; a_value (String.concat ":" (List.map Pkg.comp_to_string query.compilers))] () in
  let show_available_text = [pcdata "Show only packages available in [list of compilers separated by ':']:"] in
  let show_available = input ~a:[a_input_type `Text; a_name "show-available"; a_value (String.concat ":" (List.map Pkg.comp_to_string query.show_available))] () in
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
