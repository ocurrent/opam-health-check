open Containers
open Lwt.Infix

module Pkgs = Map.Make (String)

type state = Good | Bad

type comp = string
type pkg = string
type pkgs = (pkg * (comp * state) list) list

type query = {
  compilers : comp list;
  show_available : comp list;
}

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
  }

let pkg_update ~comp v pkgs pkg =
  let aux = function
    | None -> Some [(comp, v)]
    | Some l -> Some ((comp, v) :: l)
  in
  Pkgs.update pkg aux pkgs

let get_pkgs_from_dir workdir pkgs comp =
  get_files (Server_workdirs.gooddir ~switch:comp workdir) >>= fun good_files ->
  get_files (Server_workdirs.baddir ~switch:comp workdir) >|= fun bad_files ->
  let pkgs = List.fold_left (pkg_update ~comp Good) pkgs good_files in
  List.fold_left (pkg_update ~comp Bad) pkgs bad_files

let get_pkgs workdir compilers =
  Lwt_list.fold_left_s (get_pkgs_from_dir workdir) Pkgs.empty compilers >|= fun pkgs ->
  let pkgs = Pkgs.bindings pkgs in
  List.sort (fun (x, _) (y, _) -> OpamVersionCompare.compare x y) pkgs

let instance_to_html ~pkg instances comp =
  let open Tyxml.Html in
  let td c = td ~a:[a_class ["result-col"; "results-cell"]; a_style ("background-color: "^c^";")] in
  match List.Assoc.get ~eq:String.equal comp instances with
  | Some Good -> td "green" [pcdata "☑"]
  | Some Bad -> td "red" [a ~a:[a_href ("/"^comp^"/bad/"^pkg)] [pcdata "☒"]]
  | None -> td "grey" [pcdata "☐"]

let is_pkg_available instances compilers =
  List.exists (fun dir -> List.Assoc.mem ~eq:String.equal dir instances) compilers

let pkg_to_html {compilers; show_available} (pkg, instances) =
  let open Tyxml.Html in
  let td = td ~a:[a_class ["results-cell"]] in
  if is_pkg_available instances show_available then
    Some (tr (td [pcdata pkg] :: List.map (instance_to_html ~pkg instances) compilers))
  else
    None

let gen_table_form l =
  let open Tyxml.Html in
  let aux (txt, elt) = tr [td txt; td [elt]] in
  let legend = legend [b [pcdata "Filter form:"]] in
  form [fieldset ~legend [table (List.map aux l)]]

let get_html query pkgs =
  let open Tyxml.Html in
  (* TODO: Handle cases where there is no compilers and the following
     line will raise an exception *)
  let col_width = string_of_int (100 / List.length query.compilers) in
  let pkgs = List.filter_map (pkg_to_html query) pkgs in
  let th ?(a=[]) = th ~a:(a_class ["results-cell"]::a) in
  let dirs = th [] :: List.map (fun comp -> th ~a:[a_class ["result-col"]] [pcdata comp]) query.compilers in
  let title = title (pcdata "opam-check-all") in
  let charset = meta ~a:[a_charset "utf-8"] () in
  let style_table = pcdata ".results {border-collapse: collapse; min-width: 100%;}" in
  let style_col = pcdata (".result-col {text-align: center; width: "^col_width^"%;}") in
  let style_case = pcdata ".results-cell {border: 2px solid black;}" in
  let head = head title [charset; style [style_table; style_col; style_case]] in
  let show_available_text = [pcdata "Show only packages available in [list of compilers separated by ':']:"] in
  let show_available = input ~a:[a_input_type `Text; a_name "show-available"] () in
  let submit_form = input ~a:[a_input_type `Submit; a_value "Submit"] () in
  let filter_form = gen_table_form [(show_available_text, show_available); ([], submit_form)] in
  let doc = table ~a:[a_class ["results"]] ~thead:(thead [tr dirs]) pkgs in
  let doc = html head (body [filter_form; br (); doc]) in
  Format.sprintf "%a\n" (pp ()) doc

let comp_from_string x = x
let comp_equal = String.equal
