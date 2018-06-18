open Lwt.Infix

let serv_string ~content_type body =
  let headers = Cohttp.Header.init_with "Content-Type" content_type in
  Cohttp_lwt_unix.Server.respond_string ~headers ~status:`OK ~body ()

let serv_file ~content_type workdir file =
  let headers = Cohttp.Header.init_with "Content-Type" content_type in
  let fname = Fpath.to_string (Server_workdirs.file_from_logdir ~file workdir) in
  Cohttp_lwt_unix.Server.respond_file ~headers ~fname ()

let option_to_string = function
  | None -> ""
  | Some s -> s

let parse_raw_query workdir uri =
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
  let logdir = Server_workdirs.logdir workdir in
  begin match compilers with
  | [] | [""] -> Diff.get_dirs logdir
  | compilers -> Lwt.return (List.map Diff.comp_from_string compilers)
  end >>= fun compilers ->
  let show_available = match show_available with
    | [] | [""] -> compilers
    | show_available -> List.map Diff.comp_from_string show_available
  in
  Lwt.return {
    Diff.compilers;
    Diff.show_available;
    Diff.show_failures_only;
    Diff.show_diff_only;
    Diff.show_latest_only;
    Diff.maintainers;
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

let callback workdir _conn req _body =
  let uri = Cohttp.Request.uri req in
  match path_from_uri uri with
  | [] ->
      parse_raw_query workdir uri >>= fun query ->
      Cache.get_html query >>= fun html ->
      serv_string ~content_type:"text/html" html
  | path ->
      serv_file ~content_type:"text/plain" workdir (String.concat Fpath.dir_sep path)

let tcp_server port callback =
  Cohttp_lwt_unix.Server.create
    ~on_exn:(fun _ -> ())
    ~mode:(`TCP (`Port port))
    (Cohttp_lwt_unix.Server.make ~callback ())

let main workdir =
  Lwt_main.run begin
    Nocrypto_entropy_lwt.initialize () >>= fun () ->
    let workdir = Server_workdirs.create ~workdir in
    Server_workdirs.init_base workdir >>= fun () ->
    let conf = Server_configfile.from_workdir workdir in
    let port = Server_configfile.port conf in
    let admin_port = Server_configfile.admin_port conf in
    let callback = callback workdir in
    let admin_callback = Admin.callback workdir in
    Admin.create_admin_key workdir >>= fun () ->
    Cache.clear_and_init workdir;
    Lwt.join [
      tcp_server port callback;
      tcp_server admin_port admin_callback;
    ]
  end

let term =
  let ($) = Cmdliner.Term.($) in
  Cmdliner.Term.pure main $
  Cmdliner.Arg.(required & pos 0 (some string) None & info ~docv:"WORKDIR" [])

let info =
  Cmdliner.Term.info
    ~doc:"A server to check for broken opam packages."
    ~man:[`P "This program takes a work directory where every files created \
              are stored. This includes logs, config file and user private \
              keys."]
    ~version:Config.version
    Config.name

let () = Cmdliner.Term.exit (Cmdliner.Term.eval (term, info))
