open Lwt.Infix

module Make (Backend : Backend_intf.S) = struct
module Cache = Backend.Cache

let serv_text ~content_type body =
  let headers = Cohttp.Header.init_with "Content-Type" content_type in
  Cohttp_lwt_unix.Server.respond_string ~headers ~status:`OK ~body ()

let option_to_string = function
  | None -> ""
  | Some s -> s

let parse_raw_query uri =
  let compilers = option_to_string (Uri.get_query_param uri "compilers") in
  let compilers = String.split_on_char ':' compilers in
  let show_available = option_to_string (Uri.get_query_param uri "show-available") in
  let show_available = String.split_on_char ':' show_available in
  let show_failures_only = option_to_string (Uri.get_query_param uri "show-failures-only") in
  let show_failures_only = if String.is_empty show_failures_only then false else bool_of_string show_failures_only in
  let show_diff_only = option_to_string (Uri.get_query_param uri "show-diff-only") in
  let show_diff_only = if String.is_empty show_diff_only then false else bool_of_string show_diff_only in
  let show_latest_only = option_to_string (Uri.get_query_param uri "show-latest-only") in
  let show_latest_only = if String.is_empty show_latest_only then false else bool_of_string show_latest_only in
  let maintainers = option_to_string (Uri.get_query_param uri "maintainers") in
  let maintainers = (maintainers, Re.Posix.compile_pat ~opts:[`ICase] maintainers) in
  Cache.get_compilers () >>= fun available_compilers ->
  let compilers = match compilers with
    | [] | [""] -> available_compilers
    | compilers -> List.map Intf.Compiler.from_string compilers
  in
  let show_available = match show_available with
    | [] | [""] -> compilers
    | show_available -> List.map Intf.Compiler.from_string show_available
  in
  Lwt.return {
    Html.available_compilers;
    Html.compilers;
    Html.show_available;
    Html.show_failures_only;
    Html.show_diff_only;
    Html.show_latest_only;
    Html.maintainers;
  }

let filter_path path =
  let path = List.filter (fun file -> not (String.is_empty file)) path in
  if not (List.for_all Oca_lib.is_valid_filename path) then
    failwith "Forbidden path";
  path

let path_from_uri uri =
  match Uri.path uri with
  | "" -> []
  | path -> filter_path (Fpath.segs (Fpath.v path))

let callback backend _conn req _body =
  let uri = Cohttp.Request.uri req in
  match path_from_uri uri with
  | [] ->
      parse_raw_query uri >>= fun query ->
      Cache.get_html query >>= fun html ->
      serv_text ~content_type:"text/html" html
  | [comp; state; pkg] ->
      let comp = Intf.Compiler.from_string comp in
      let state = Intf.State.from_string state in
      Backend.get_log backend ~comp ~state ~pkg >>= fun log ->
      serv_text ~content_type:"text/plain; charset=utf-8" log
  | _ ->
      failwith "path non recognized: 404"

let tcp_server port callback =
  Cohttp_lwt_unix.Server.create
    ~on_exn:(fun _ -> ())
    ~mode:(`TCP (`Port port))
    (Cohttp_lwt_unix.Server.make ~callback ())

let main ~workdir =
  let workdir = Server_workdirs.create ~workdir in
  Server_workdirs.init_base workdir >>= fun () ->
  let conf = Server_configfile.from_workdir workdir in
  let port = Server_configfile.port conf in
  Backend.start conf workdir >>= fun (backend, backend_task) ->
  Cache.clear_and_init backend;
  Lwt.join [
    tcp_server port (callback backend);
    backend_task ();
  ]
end
