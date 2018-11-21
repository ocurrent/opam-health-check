open Intf

type query = {
  available_compilers : Compiler.t list;
  compilers : Compiler.t list;
  show_available : Compiler.t list;
  show_failures_only : bool;
  show_diff_only : bool;
  show_latest_only : bool;
  maintainers : string * Re.re;
}

let log_url pkg instance =
  let comp = Instance.compiler instance in
  let comp = Compiler.to_string comp in
  let state = State.to_string (Instance.state instance) in
  let pkg = Pkg.full_name pkg in
  Printf.sprintf "/%s/%s/%s" comp state pkg

let instance_to_html ~pkg instances comp =
  let open Tyxml.Html in
  let td c = td ~a:[a_class ["result-col"; "results-cell"]; a_style ("background-color: "^c^";")] in
  match List.find_opt (fun i -> Compiler.equal (Instance.compiler i) comp) instances with
  | Some instance ->
      begin match Instance.state instance with
      | State.Good -> td "green" [a ~a:[a_href (log_url pkg instance)] [pcdata "☑"]]
      | State.Partial -> td "orange" [a ~a:[a_href (log_url pkg instance)] [pcdata "☒"]]
      | State.Bad -> td "red" [a ~a:[a_href (log_url pkg instance)] [pcdata "☒"]]
      end
  | None -> td "grey" [pcdata "☐"]

let must_show_package query ~last pkg =
  let maintainers = Pkg.maintainers pkg in
  let instances = Pkg.instances pkg in
  List.exists (fun comp -> List.exists (fun instance -> Compiler.equal comp (Instance.compiler instance)) instances) query.show_available &&
  let instances = List.filter (fun inst -> List.mem ~eq:Intf.Compiler.equal (Intf.Instance.compiler inst) query.compilers) instances in
  begin
    if query.show_failures_only then
      List.exists (fun instance -> match Instance.state instance with
        | State.Bad | State.Partial -> true
        | State.Good -> false
      ) instances
    else
      true
  end &&
  begin
    match instances with
    | hd::tl when query.show_diff_only ->
        let state = Instance.state hd in
        List.exists (fun x -> not (State.equal state (Instance.state x))) tl
    | [] | _::_ ->
        true
  end &&
  begin
    if query.show_latest_only then
      match last with
      | None -> true
      | Some last -> not (String.equal (Pkg.name pkg) (Pkg.name last))
    else
      true
  end &&
  begin
    if not (String.is_empty (fst query.maintainers)) then
      List.exists (Re.execp (snd query.maintainers)) maintainers
    else
      true
  end

let pkg_to_html query (acc, last) pkg =
  let open Tyxml.Html in
  let tr = tr ~a:[a_class ["results-row"]] in
  let td = td ~a:[a_class ["results-cell"; "pkgname"]] in
  if must_show_package query ~last pkg then
    ((tr (td [pcdata (Pkg.full_name pkg)] :: List.map (instance_to_html ~pkg (Pkg.instances pkg)) query.compilers)) :: acc, Some pkg)
  else
    (acc, last)

let result_legend query =
  let open Tyxml.Html in
  let legend = legend [b [pcdata "Legend:"]] in
  fieldset ~legend [table ~a:[a_style "white-space: nowrap;"] [
    tr [td [pcdata "Available compilers:"]; td [pcdata (String.concat ", " (List.map Compiler.to_string query.available_compilers))]];
    tr [td ~a:[a_style "background-color: green; text-align: center;"] [pcdata "☑"]; td [pcdata "Package successfully built"]];
    tr [td ~a:[a_style "background-color: orange; text-align: center;"] [pcdata "☒"]; td [pcdata "One of the dependencies failed to build"]];
    tr [td ~a:[a_style "background-color: red; text-align: center;"] [pcdata "☒"]; td [pcdata "Package failed to build"]];
    tr [td ~a:[a_style "background-color: grey; text-align: center;"] [pcdata "☐"]; td [pcdata "Package is not available in this environment"]];
  ]]

let gen_table_form ~conf query l =
  let open Tyxml.Html in
  let aux (txt, elt) = tr [td txt; td [elt]] in
  let legend = legend [b [pcdata "Filter form:"]] in
  let opam_repo_uri = match Server_configfile.opam_repo_commit_hash conf with
    | None -> b [pcdata "undefined opam-repository commit hash"]
    | Some hash -> a ~a:[a_href ("https://github.com/ocaml/opam-repository/commit/"^hash)] [b [pcdata "🔗 opam-repository commit hash"]]
  in
  form [fieldset ~legend [table [tr [
    td ~a:[a_style "width: 100%;"] [table (List.map aux l)];
    td [result_legend query;
        p ~a:[a_style "text-align: right;"] [opam_repo_uri];
       ]
  ]]]]

let get_html ~conf query pkgs =
  let open Tyxml.Html in
  let col_width = string_of_int (100 / max 1 (List.length query.compilers)) in
  let pkgs, _ = List.fold_left (pkg_to_html query) ([], None) (List.rev pkgs) in
  let th ?(a=[]) = th ~a:(a_class ["results-cell"]::a) in
  let dirs = th [] :: List.map (fun comp -> th ~a:[a_class ["result-col"]] [pcdata (Compiler.to_string comp)]) query.compilers in
  let title = title (pcdata "opam-check-all") in
  let charset = meta ~a:[a_charset "utf-8"] () in
  let style_table = pcdata ".results {border: 2px solid black; border-collapse: collapse; min-width: 100%;}" in
  let style_col = pcdata (".result-col {text-align: center; width: "^col_width^"%;}") in
  let style_case = pcdata ".results-cell {border-left: 2px solid black;}" in
  let style_row = pcdata ".results-row {border-top: 2px solid black; border-bottom: 2px solid black;}" in
  let style_row_hover = pcdata ".results-row:hover {border-top-width: 4px; border-bottom-width: 4px;}" in
  let style_pkgname = pcdata ".pkgname {white-space: nowrap;}" in
  let style_a = pcdata "a {text-decoration: none;}" in
  let head = head title [charset; style [style_table; style_col; style_case; style_pkgname; style_row; style_row_hover; style_a]] in
  let compilers_text = [pcdata "Show only [list of compilers separated by ':']:"] in
  let compilers = input ~a:[a_input_type `Text; a_name "compilers"; a_value (String.concat ":" (List.map Compiler.to_string query.compilers))] () in
  let show_available_text = [pcdata "Show only packages available in [list of compilers separated by ':']:"] in
  let show_available = input ~a:[a_input_type `Text; a_name "show-available"; a_value (String.concat ":" (List.map Compiler.to_string query.show_available))] () in
  let show_failures_only_text = [pcdata "Show failures only:"] in
  let show_failures_only = input ~a:(a_input_type `Checkbox::a_name "show-failures-only"::a_value "true"::if query.show_failures_only then [a_checked ()] else []) () in
  let show_diff_only_text = [pcdata "Only show packages that have different build status between each compilers:"] in
  let show_diff_only = input ~a:(a_input_type `Checkbox::a_name "show-diff-only"::a_value "true"::if query.show_diff_only then [a_checked ()] else []) () in
  let show_latest_only_text = [pcdata "Only show the latest version of each packages:"] in
  let show_latest_only = input ~a:(a_input_type `Checkbox::a_name "show-latest-only"::a_value "true"::if query.show_latest_only then [a_checked ()] else []) () in
  let maintainers_text = [pcdata "Show only packages maintained by [posix regexp]:"] in
  let maintainers = input ~a:[a_input_type `Text; a_name "maintainers"; a_value (fst query.maintainers)] () in
  let submit_form = input ~a:[a_input_type `Submit; a_value "Submit"] () in
  let filter_form = gen_table_form ~conf query [
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
