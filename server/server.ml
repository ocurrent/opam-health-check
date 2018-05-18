open Containers
open Lwt.Infix

module Path : sig
  type t = string list

  val of_uri : Uri.t -> t
  val to_string : t -> string
end = struct
  type elt = string
  type t = elt list

  let rec normalize_path = function
    | [] -> []
    | ""::xs -> normalize_path xs
    | x::_ when String.equal x Filename.parent_dir_name -> failwith "You bastard !"
    | x::_ when String.mem ~sub:Filename.dir_sep x -> failwith "You bastard !!"
    | x::xs -> x :: normalize_path xs

  let of_uri path =
    normalize_path (String.split_on_char '/' (Uri.path path))

  let to_string path =
    String.concat Filename.dir_sep path
end

let serv_string ~content_type body =
  let headers = Cohttp.Header.init_with "Content-Type" content_type in
  Cohttp_lwt_unix.Server.respond_string ~headers ~status:`OK ~body ()

let serv_file ~content_type ~logdir file =
  let headers = Cohttp.Header.init_with "Content-Type" content_type in
  let fname = Filename.concat logdir file in
  Cohttp_lwt_unix.Server.respond_file ~headers ~fname ()

let callback ~logdir _conn req _body =
  match Path.of_uri (Cohttp.Request.uri req) with
  | [] ->
      Cache.get_html ~logdir [] >>= fun html ->
      serv_string ~content_type:"text/html" html
  | "diff"::compilers ->
      let compilers = List.map Diff.comp_from_string compilers in
      Cache.get_html ~logdir compilers >>= fun html ->
      serv_string ~content_type:"text/html" html
  | path ->
      serv_file ~content_type:"text/plain" ~logdir (Path.to_string path)

let tcp_server port callback =
  Cohttp_lwt_unix.Server.create
    ~on_exn:(fun _ -> ())
    ~mode:(`TCP (`Port port))
    (Cohttp_lwt_unix.Server.make ~callback ())

let main workdir =
  Lwt_main.run begin
    Nocrypto_entropy_lwt.initialize () >>= fun () ->
    Oca_lib.mkdir_p workdir >>= fun () ->
    let conf = Server_configfile.from_workdir workdir in
    let logdir = Filename.concat workdir "logs" in
    let ilogdir = Filename.concat workdir "ilogs" in
    let keysdir = Oca_lib.keysdir ~workdir in
    Oca_lib.mkdir_p logdir >>= fun () ->
    Oca_lib.mkdir_p ilogdir >>= fun () ->
    Oca_lib.mkdir_p keysdir >>= fun () ->
    let port = Server_configfile.port conf in
    let admin_port = Server_configfile.admin_port conf in
    let callback = callback ~logdir in
    let admin_callback = Admin.callback ~logdir ~ilogdir ~keysdir in
    Admin.create_admin_key ~keysdir >>= fun () ->
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
