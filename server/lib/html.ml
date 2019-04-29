open Lwt.Infix
open Intf

type query = {
  available_compilers : Compiler.t list;
  compilers : Compiler.t list;
  show_available : Compiler.t list;
  show_failures_only : bool;
  show_diff_only : bool;
  show_latest_only : bool;
  sort_by_revdeps : bool;
  maintainers : string * Re.re option;
  logsearch : string * (Re.re * Compiler.t) option;
}

let github_url = "https://github.com/ocaml/opam-repository"

(* NOTE: Attempt at finding the right set of colors unambiguous to both colorblinds and non-colorblinds.
   Base work:
   * http://jfly.iam.u-tokyo.ac.jp/color/
   * http://jfly.iam.u-tokyo.ac.jp/colorset/CUD_color_set_ver3_vs_ver2.jpg
   Tool used to tweek the colors:
   * https://addons.mozilla.org/en-US/firefox/addon/let-s-get-color-blind/
*)
module CUD_pallette = struct
  let red = "#ff2800"
  let orange = "#ffdc32"
  let green = "#64e178"
  let grey = "#929292"
end

let log_url pkg instance =
  let comp = Instance.compiler instance in
  let comp = Compiler.to_string comp in
  let state = State.to_string (Instance.state instance) in
  let pkg = Pkg.full_name pkg in
  Printf.sprintf "/%s/%s/%s" comp state pkg

let instance_to_html ~pkg instances comp =
  let open Tyxml.Html in
  let td c = td ~a:[a_class ["result-col"; "results-cell"; c]] in
  match List.find_opt (fun i -> Compiler.equal (Instance.compiler i) comp) instances with
  | Some instance ->
      begin match Instance.state instance with
      | State.Good -> td "cell-good" [a ~a:[a_href (log_url pkg instance)] [txt "☑"]]
      | State.Partial -> td "cell-partial" [a ~a:[a_href (log_url pkg instance)] [txt "☒"]]
      | State.Bad -> td "cell-bad" [a ~a:[a_href (log_url pkg instance)] [txt "☒"]]
      | State.NotAvailable -> td "cell-not-available" [a ~a:[a_href (log_url pkg instance)] [txt "☒"]]
      | State.InternalFailure -> td "cell-internal-failure" [a ~a:[a_href (log_url pkg instance)] [txt "☒"]]
      end
  | None -> td "cell-not-available" [txt "☐"] (* NOTE: Should not happen in the new versions but can happen with old data or custom runs *)

let (>>&) x f =
  if x then f () else Lwt.return_false

let (>>&&) x f =
  x >>= fun x ->
  if x then f () else Lwt.return_false

let must_show_package query ~last pkg =
  let maintainers = Pkg.maintainers pkg in
  let instances = Pkg.instances pkg in
  List.exists (fun comp -> List.exists (fun instance -> Compiler.equal comp (Instance.compiler instance)) instances) query.show_available >>& fun () ->
  let instances = List.filter (fun inst -> List.mem ~eq:Intf.Compiler.equal (Intf.Instance.compiler inst) query.compilers) instances in
  begin
    Lwt.return @@
    if query.show_failures_only then
      List.exists (fun instance -> match Instance.state instance with
        | State.Bad | State.Partial -> true
        | State.Good | State.NotAvailable | State.InternalFailure -> false
      ) instances
    else
      true
  end >>&& begin fun () ->
    Lwt.return @@
    match instances with
    | hd::tl when query.show_diff_only ->
        let state = Instance.state hd in
        List.exists (fun x -> not (State.equal state (Instance.state x))) tl
    | [] | _::_ ->
        true
  end >>&& begin fun () ->
    Lwt.return @@
    if query.show_latest_only then
      match last with
      | None -> true
      | Some last -> not (String.equal (Pkg.name pkg) (Pkg.name last))
    else
      true
  end >>&& begin fun () ->
    Lwt.return @@
    match snd query.maintainers with
    | Some re -> List.exists (Re.execp re) maintainers
    | None -> true
  end >>&& begin fun () ->
    match snd query.logsearch with
    | Some (re, comp) ->
        Lwt_main.yield () >>= fun () ->
        Lwt_list.exists_s begin fun inst ->
          if Intf.Compiler.equal comp (Intf.Instance.compiler inst) then
            Intf.Instance.content inst >|= Re.execp re
          else
            Lwt.return_false
        end instances
    | None ->
        Lwt.return_true
  end

let filter_pkg query (acc, last) pkg =
  must_show_package query ~last pkg >|= function
  | true -> (pkg :: acc, Some pkg)
  | false -> (acc, last)

let pkg_to_html query pkg =
  let open Tyxml.Html in
  let tr = tr ~a:[a_class ["results-row"]] in
  let td ?(a=[]) = td ~a:(a_class ["results-cell"; "pkgname"]::a) in
  let revdeps = td ~a:[a_style "text-align: center;"] [txt (string_of_int (Pkg.revdeps pkg))] in
  tr (td [txt (Pkg.full_name pkg)] :: List.map (instance_to_html ~pkg (Pkg.instances pkg)) query.compilers@ [revdeps])

let result_legend query =
  let open Tyxml.Html in
  let legend = legend [b [txt "Legend:"]] in
  fieldset ~legend [table ~a:[a_style "white-space: nowrap;"] [
    tr [td [txt "Available compilers:"]; td [txt (String.concat ", " (List.map Compiler.to_string query.available_compilers))]];
    tr [td ~a:[a_class ["cell-good"]] [txt "☑"]; td [txt "Package successfully built"]];
    tr [td ~a:[a_class ["cell-partial"]] [txt "☒"]; td [txt "One of the dependencies failed to build"]];
    tr [td ~a:[a_class ["cell-bad"]] [txt "☒"]; td [txt "Package failed to build"]];
    tr [td ~a:[a_class ["cell-not-available"]] [txt "☒"]; td [txt "Package is not available in this environment"]];
    tr [td ~a:[a_class ["cell-internal-failure"]; a_style "border: 2px solid black;"] [txt "☒"]; td [txt "Internal failure"]];
  ]]

let get_opam_repository_commit_url ~hash ~content =
  let open Tyxml.Html in
  a ~a:[a_href (github_url^"/commit/"^hash)] content

let gen_table_form ~conf query l =
  let open Tyxml.Html in
  let aux (txt, elts) = tr [td txt; td elts] in
  let legend = legend [b [txt "Filter form:"]] in
  let opam_repo_uri = match Server_configfile.opam_repo_commit_hash conf with
    | None -> b [txt "undefined opam-repository commit hash"]
    | Some hash -> get_opam_repository_commit_url ~hash ~content:[b [txt "🔗 opam-repository commit hash"]]
  in
  let opam_diff_uri = a ~a:[a_href "/diff"] [b [txt "🔗 Differences with the last check"]] in
  form [fieldset ~legend [table [tr [
    td ~a:[a_style "width: 100%;"] [table (List.map aux l)];
    td [result_legend query;
        p ~a:[a_style "text-align: right;"] [opam_repo_uri];
        p ~a:[a_style "text-align: right;"] [opam_diff_uri];
       ]
  ]]]]

let comp_checkboxes ~name checked query =
  let open Tyxml.Html in
  List.concat begin
    List.map begin fun comp ->
      let comp_str = Compiler.to_string comp in
      [txt (" "^comp_str^": ");
       input
         ~a:(a_input_type `Checkbox ::
             a_name name ::
             a_value comp_str ::
             if List.mem ~eq:Compiler.equal comp checked then [a_checked ()] else [])
         ()]
    end query.available_compilers
  end

let revdeps_cmp p1 p2 =
  Int.neg (Int.compare (Intf.Pkg.revdeps p1) (Intf.Pkg.revdeps p2))

let get_html ~conf query pkgs =
  let open Tyxml.Html in
  let col_width = string_of_int (100 / max 1 (List.length query.compilers)) in
  Lwt_list.fold_left_s (filter_pkg query) ([], None) (List.rev pkgs) >|= fun (pkgs, _) ->
  let pkgs = if query.sort_by_revdeps then List.sort revdeps_cmp pkgs else pkgs in
  let pkgs = List.map (pkg_to_html query) pkgs in
  let th ?(a=[]) = th ~a:(a_class ["results-cell"]::a) in
  let dirs = th [] :: List.map (fun comp -> th ~a:[a_class ["result-col"]] [txt (Compiler.to_string comp)]) query.compilers @ [th [txt "number of revdeps"]] in
  let title = title (txt "opam-health-check") in
  let charset = meta ~a:[a_charset "utf-8"] () in
  let style_table = txt "#results {border: 2px solid black; border-collapse: collapse; min-width: 100%;}" in
  let style_thead = txt "#results thead tr {border-bottom: 2px solid black;}" in
  let style_col = txt (".result-col {text-align: center; width: "^col_width^"%;}") in
  let style_case = txt ".results-cell {border-left: 2px solid black;}" in
  let style_row = txt ".results-row {border-bottom: 2px solid black;}" in
  let style_pkgname = txt ".pkgname {white-space: nowrap;}" in
  let style_a = txt "a {text-decoration: none;}" in
  let style_cell_good = txt (".cell-good {background-color: "^CUD_pallette.green^"; text-align: center;}") in
  let style_cell_partial = txt (".cell-partial {background-color: "^CUD_pallette.orange^"; text-align: center;}") in
  let style_cell_bad = txt (".cell-bad {background-color: "^CUD_pallette.red^"; text-align: center;}") in
  let style_cell_not_available = txt (".cell-not-available {background-color: "^CUD_pallette.grey^"; text-align: center;}") in
  let style_cell_internal_failure = txt (".cell-internal-failure {background-color: white; text-align: center;}") in
  let javascript = Unsafe.data {|
    let x = document.getElementById("results").rows;

    for(i = 1; i < x.length; ++i) {
      let c = i;
      x[c].onmouseenter = function() {
        x[c-1].style.borderBottomStyle = "dashed";
        x[c].style.borderBottomStyle = "dashed";
      };
      x[c].onmouseleave = function() {
        x[c-1].style.borderBottomStyle = "solid";
        x[c].style.borderBottomStyle = "solid";
      };
    }
  |} in
  let head = head title [
    charset;
    style [style_table; style_thead; style_col; style_case; style_pkgname; style_row; style_a;
           style_cell_good; style_cell_partial; style_cell_bad; style_cell_not_available; style_cell_internal_failure];
  ] in
  let compilers_text = [txt "Show only:"] in
  let compilers = comp_checkboxes ~name:"comp" query.compilers query in
  let show_available_text = [txt "Show only packages available in:"] in
  let show_available = comp_checkboxes ~name:"available" query.show_available query in
  let show_failures_only_text = [txt "Show failures only:"] in
  let show_failures_only = input ~a:(a_input_type `Checkbox::a_name "show-failures-only"::a_value "true"::if query.show_failures_only then [a_checked ()] else []) () in
  let show_diff_only_text = [txt "Only show packages that have different build status between each compilers:"] in
  let show_diff_only = input ~a:(a_input_type `Checkbox::a_name "show-diff-only"::a_value "true"::if query.show_diff_only then [a_checked ()] else []) () in
  let show_latest_only_text = [txt "Only show the latest version of each packages:"] in
  let show_latest_only = input ~a:(a_input_type `Checkbox::a_name "show-latest-only"::a_value "true"::if query.show_latest_only then [a_checked ()] else []) () in
  let sort_by_revdeps_text = [txt "Sort by number of revdeps:"] in
  let sort_by_revdeps = input ~a:(a_input_type `Checkbox::a_name "sort-by-revdeps"::a_value "true"::if query.sort_by_revdeps then [a_checked ()] else []) () in
  let maintainers_text = [txt "Show only packages maintained by [posix regexp]:"] in
  let maintainers = input ~a:[a_input_type `Text; a_name "maintainers"; a_value (fst query.maintainers)] () in
  let logsearch_text = [txt "Show only packages where one of the logs matches [posix regexp]:"] in
  let logsearch = input ~a:[a_input_type `Text; a_name "logsearch"; a_value (fst query.logsearch)] () in
  let opts_comp = List.map begin fun comp ->
    let comp_str = Intf.Compiler.to_string comp in
    option
      ~a:(a_value comp_str :: match snd query.logsearch with Some (_, c) when Intf.Compiler.equal c comp -> [a_selected ()] | Some _ | None -> [])
      (txt comp_str)
  end query.compilers in
  let logsearch_comp = select ~a:[a_name "logsearch_comp"] opts_comp in
  let submit_form = input ~a:[a_input_type `Submit; a_value "Submit"] () in
  let filter_form = gen_table_form ~conf query [
    (compilers_text, compilers);
    (show_available_text, show_available);
    (show_failures_only_text, [show_failures_only]);
    (show_diff_only_text, [show_diff_only]);
    (show_latest_only_text, [show_latest_only]);
    (sort_by_revdeps_text, [sort_by_revdeps]);
    (maintainers_text, [maintainers]);
    (logsearch_text, [logsearch; logsearch_comp]);
    ([], [submit_form]);
  ] in
  let doc = table ~a:[a_id "results"] ~thead:(thead [tr dirs]) pkgs in
  let doc = html head (body [filter_form; br (); doc; script javascript]) in
  Format.sprintf "%a\n" (pp ()) doc

let generate_diff_html {Intf.Pkg_diff.full_name; comp; diff} =
  let open Tyxml.Html in
  let comp_str = Intf.Compiler.to_string comp in
  let prefix = [b [txt full_name]; txt " on "; b [txt comp_str]] in
  let good = span ~a:[a_style ("color: "^CUD_pallette.green^";")] [txt "passing"] in
  let bad = span ~a:[a_style ("color: "^CUD_pallette.red^";")] [txt "failing"] in
  let partial = span ~a:[a_style ("color: "^CUD_pallette.orange^";")] [txt "partially failing"] in
  let not_available = span ~a:[a_style ("color: "^CUD_pallette.grey^";")] [txt "not available"] in
  let internal_failure = span ~a:[a_style "border: 2px solid black;"] [txt "internal failure"] in
  let print_status = function
    | Intf.State.Good -> good
    | Intf.State.Partial -> partial
    | Intf.State.Bad -> bad
    | Intf.State.NotAvailable -> not_available
    | Intf.State.InternalFailure -> internal_failure
  in
  let get_status_elm ~old status =
    let status_str = Intf.State.to_string status in
    let status = print_status status in
    let root = if old then "/old/" else "/" in
    a ~a:[a_href (root^comp_str^"/"^status_str^"/"^full_name)] [status]
  in
  let diff = match diff with
    | Intf.Pkg_diff.StatusChanged (old_status, new_status) ->
        let old_status = get_status_elm ~old:true old_status in
        let new_status = get_status_elm ~old:false new_status in
        [txt " had its build status changed: "; old_status; txt " to "; new_status]
    | Intf.Pkg_diff.NowInstallable new_status ->
        let new_status = get_status_elm ~old:false new_status in
        [txt " is now installable. Current state is: "; new_status]
    | Intf.Pkg_diff.NotAvailableAnymore old_status ->
        let old_status = get_status_elm ~old:true old_status in
        [txt " is not available anymore. Previous state was: "; old_status]
  in
  li (prefix @ diff)

let get_diff ~conf (bad, partial, not_available, internal_failure, good) =
  let open Tyxml.Html in
  let title = title (txt "opam-health-check diff") in
  let charset = meta ~a:[a_charset "utf-8"] () in
  let head = head title [charset] in
  let get_hash_elm hash =
    let hash = String.take 7 hash in
    get_opam_repository_commit_url ~hash ~content:[b [txt hash]]
  in
  let old_hash = Option.get_exn (Server_configfile.opam_repo_old_commit_hash conf) in
  let old_hash_elm = get_hash_elm old_hash in
  let new_hash = Option.get_exn (Server_configfile.opam_repo_commit_hash conf) in
  let new_hash_elm = get_hash_elm new_hash in
  let git_diff = a ~a:[a_href (github_url^"/compare/"^old_hash^"..."^new_hash)] [txt "git diff"] in
  let good_txt = span ~a:[a_style ("color: "^CUD_pallette.green^";")] [txt "passing"] in
  let bad_txt = span ~a:[a_style ("color: "^CUD_pallette.red^";")] [txt "failing"] in
  let partial_txt = span ~a:[a_style ("color: "^CUD_pallette.orange^";")] [txt "partially failing"] in
  let not_available_txt = span ~a:[a_style ("color: "^CUD_pallette.grey^";")] [txt "not available"] in
  let internal_failure_txt = span ~a:[a_style "border: 2px solid black;"] [txt "internal failure"] in
  let doc = html head (body [
    h2 [txt "Differences between "; old_hash_elm; txt " and "; new_hash_elm; txt " ("; git_diff; txt ")"];
    br ();
    h3 [txt "Packages now ";bad_txt;txt ":"];
    ul (List.map generate_diff_html bad);
    br ();
    h3 [txt "Packages now ";partial_txt; txt ":"];
    ul (List.map generate_diff_html partial);
    br ();
    h3 [txt "Packages now ";not_available_txt; txt ":"];
    ul (List.map generate_diff_html not_available);
    br ();
    h3 [txt "Packages now ";internal_failure_txt; txt ":"];
    ul (List.map generate_diff_html internal_failure);
    br ();
    h3 [txt "Packages now :";good_txt; txt ":"];
    ul (List.map generate_diff_html good);
  ]) in
  Format.sprintf "%a\n" (pp ()) doc
