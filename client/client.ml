let parse_key key =
  let key = IO.with_in (Fpath.to_string key) (IO.read_all ?size:None) in
  let key =
    try Mirage_crypto_pk.Rsa.priv_of_sexp (Sexplib.Sexp.of_string key) with
      Sexplib0.Sexp_conv.Of_sexp_error _ ->
      match X509.Private_key.decode_pem (Cstruct.of_string key) with
      | Ok `RSA key -> key
      | Ok _ -> failwith "unsupported key type, only RSA supported"
      | Error `Msg m -> failwith ("error decoding key: " ^ m)
  in
  Mirage_crypto_pk.Rsa.pub_of_priv key

let partial_encrypt key msg =
  Cstruct.to_string (Mirage_crypto_pk.Rsa.encrypt ~key (Cstruct.of_string msg))

let rec encrypt_msg ~key msg =
  let max_size = Mirage_crypto_pk.Rsa.pub_bits key / 8 in
  if String.length msg <= max_size then
    partial_encrypt key msg
  else
    let msg, next = String.take_drop max_size msg in
    partial_encrypt key msg ^ encrypt_msg ~key next

let print_body body =
  let stream = Cohttp_lwt.Body.to_stream body in
  let%lwt () = Lwt_stream.iter (fun s -> print_string s; flush stdout) stream in
  Lwt.return (print_newline ())

let process_response (res, body) =
  match Cohttp.Response.status res with
  | `OK ->
      print_body body
  | `Upgrade_required ->
      let%lwt () = print_body body in
      Lwt.fail Exit
  | _ ->
      print_endline "A problem occured";
      Lwt.fail Exit

let send_msg ~profilename ~confdir ~conffile msg =
  let conf = Configfile.from_file ~confdir conffile in
  let conf = Configfile.profile ~profilename conf in
  let hostname = Configfile.hostname conf in
  let port = Configfile.port conf in
  let username = Configfile.username conf in
  let keyfile = Configfile.keyfile conf in
  let key = parse_key keyfile in
  let prefix = username^"\n" in
  let msg = String.concat "\n" msg in
  let msg = encrypt_msg ~key (prefix^msg) in
  let uri = Uri.make ~scheme:"http" ~host:hostname ~port () in
  let prefix = Oca_lib.protocol_version^"\n"^prefix in
  print_endline "Sending command…";
  Lwt_main.run begin
    let%lwt resp = Cohttp_lwt_unix.Client.post ~body:(`String (prefix^msg)) uri in
    process_response resp
  end

let set_auto_run_interval ~confdir ~conffile profilename i =
  send_msg ~profilename ~confdir ~conffile ["set-auto-run-interval"; i]

module Arg = Cmdliner.Arg
module Cmd = Cmdliner.Cmd
module Term = Cmdliner.Term

let ( $ ) = Term.( $ )
let ( & ) = Arg.( & )

let set_auto_run_interval_cmd ~confdir ~conffile =
  let term =
    Term.const (set_auto_run_interval ~confdir ~conffile) $
    (Arg.value & Arg.opt Arg.string "default" & Arg.info ~docv:"PROFILENAME" ["profile"; "p"]) $
    (Arg.required & Arg.pos 0 (Arg.some Arg.string) None & Arg.info ~docv:"HOURS" [])
  in
  let info = Cmd.info "set-auto-run-interval" in
  Cmd.v info term

let set_processes ~confdir ~conffile profilename i =
  send_msg ~profilename ~confdir ~conffile ["set-processes"; i]

let set_processes_cmd ~confdir ~conffile =
  let term =
    Term.const (set_processes ~confdir ~conffile) $
    (Arg.value & Arg.opt Arg.string "default" & Arg.info ~docv:"PROFILENAME" ["profile"; "p"]) $
    (Arg.required & Arg.pos 0 (Arg.some Arg.string) None & Arg.info ~docv:"NAT" [])
  in
  let info = Cmd.info "set-processes" in
  Cmd.v info term

let add_ocaml_switch ~confdir ~conffile profilename name switch =
  send_msg ~profilename ~confdir ~conffile ["add-ocaml-switch";name;switch]

let add_ocaml_switch_cmd ~confdir ~conffile =
  let term =
    Term.const (add_ocaml_switch ~confdir ~conffile) $
    (Arg.value & Arg.opt Arg.string "default" & Arg.info ~docv:"PROFILENAME" ["profile"; "p"]) $
    (Arg.required & Arg.pos 0 (Arg.some Arg.string) None & Arg.info ~docv:"NAME" []) $
    (Arg.required & Arg.pos 1 (Arg.some Arg.string) None & Arg.info ~docv:"SWITCH" [])
  in
  let info = Cmd.info "add-ocaml-switch" in
  Cmd.v info term

let set_ocaml_switch ~confdir ~conffile profilename name switch =
  send_msg ~profilename ~confdir ~conffile ["set-ocaml-switch";name;switch]

let set_ocaml_switch_cmd ~confdir ~conffile =
  let term =
    Term.const (set_ocaml_switch ~confdir ~conffile) $
    (Arg.value & Arg.opt Arg.string "default" & Arg.info ~docv:"PROFILENAME" ["profile"; "p"]) $
    (Arg.required & Arg.pos 0 (Arg.some Arg.string) None & Arg.info ~docv:"NAME" []) $
    (Arg.required & Arg.pos 1 (Arg.some Arg.string) None & Arg.info ~docv:"SWITCH" [])
  in
  let info = Cmd.info "set-ocaml-switch" in
  Cmd.v info term

let rm_ocaml_switch ~confdir ~conffile profilename name =
  send_msg ~profilename ~confdir ~conffile ["rm-ocaml-switch";name]

let rm_ocaml_switch_cmd ~confdir ~conffile =
  let term =
    Term.const (rm_ocaml_switch ~confdir ~conffile) $
    (Arg.value & Arg.opt Arg.string "default" & Arg.info ~docv:"PROFILENAME" ["profile"; "p"]) $
    (Arg.required & Arg.pos 0 (Arg.some Arg.string) None & Arg.info ~docv:"NAME" [])
  in
  let info = Cmd.info "rm-ocaml-switch" in
  Cmd.v info term

let set_slack_webhooks ~confdir ~conffile profilename webhooks =
  send_msg ~profilename ~confdir ~conffile ("set-slack-webhooks"::webhooks)

let set_slack_webhooks_cmd ~confdir ~conffile =
  let term =
    Term.const (set_slack_webhooks ~confdir ~conffile) $
    (Arg.value & Arg.opt Arg.string "default" & Arg.info ~docv:"PROFILENAME" ["profile"; "p"]) $
    (Arg.value & Arg.pos_all Arg.string [] & Arg.info ~docv:"WEBHOOKS" [])
  in
  let info = Cmd.info "set-slack-webhooks" in
  Cmd.v info term

let set_list_command ~confdir ~conffile profilename cmd =
  send_msg ~profilename ~confdir ~conffile ["set-list-command";cmd]

let set_list_command_cmd ~confdir ~conffile =
  let term =
    Term.const (set_list_command ~confdir ~conffile) $
    (Arg.value & Arg.opt Arg.string "default" & Arg.info ~docv:"PROFILENAME" ["profile"; "p"]) $
    (Arg.required & Arg.pos 0 (Arg.some Arg.string) None & Arg.info ~docv:"CMD" [])
  in
  let info = Cmd.info "set-list-command" in
  Cmd.v info term

let run ~confdir ~conffile profilename () =
  (* TODO: Catch the exception if the config file doesn't exist *)
  send_msg ~profilename ~confdir ~conffile ["run"]

let run_cmd ~confdir ~conffile =
  let term =
    Term.const (run ~confdir ~conffile) $
    (Arg.value & Arg.opt Arg.string "default" & Arg.info ~docv:"PROFILENAME" ["profile"; "p"]) $
    Term.const ()
  in
  let info = Cmd.info "run" in
  Cmd.v info term

let add_user ~confdir ~conffile profilename username =
  send_msg ~profilename ~confdir ~conffile ["add-user";username]

let add_user_cmd ~confdir ~conffile =
  let term =
    Term.const (add_user ~confdir ~conffile) $
    (Arg.value & Arg.opt Arg.string "default" & Arg.info ~docv:"PROFILENAME" ["profile"; "p"]) $
    (Arg.required & Arg.pos 0 (Arg.some Arg.string) None & Arg.info ~docv:"USERNAME" [])
  in
  let info = Cmd.info "add-user" in
  Cmd.v info term

let init ~confdir ~conffile = function
  | Some local_workdir ->
      let cwd = Sys.getcwd () in
      let local_workdir = Server_workdirs.create ~cwd ~workdir:local_workdir in
      let server_conf = Server_configfile.from_workdir local_workdir in
      let profilename = Server_configfile.name server_conf in
      let hostname = Oca_lib.localhost in
      let port = Server_configfile.admin_port server_conf in
      let username = Oca_lib.default_admin_name in
      let keyfile = Server_workdirs.keyfile ~username local_workdir in
      Configfile.init_with_values ~confdir ~profilename ~hostname ~port ~username ~keyfile conffile
  | None ->
      Configfile.init ~confdir conffile

let init_cmd ~confdir ~conffile =
  let term =
    Term.const (init ~confdir ~conffile) $
    (Arg.value & Arg.opt (Arg.some Arg.dir) None & Arg.info ["from-local-workdir"])
  in
  let info = Cmd.info "init" in
  Cmd.v info term

let clear_cache ~confdir ~conffile profilename () =
  send_msg ~profilename ~confdir ~conffile ["clear-cache"]

let clear_cache_cmd ~confdir ~conffile =
  let term =
    Term.const (clear_cache ~confdir ~conffile) $
    (Arg.value & Arg.opt Arg.string "default" & Arg.info ~docv:"PROFILENAME" ["profile"; "p"]) $
    Term.const ()
  in
  let info = Cmd.info "clear-cache" in
  Cmd.v info term

let log ~confdir ~conffile profilename () =
  send_msg ~profilename ~confdir ~conffile ["log"]

let log_cmd ~confdir ~conffile =
  let term =
    Term.const (log ~confdir ~conffile) $
    (Arg.value & Arg.opt Arg.string "default" & Arg.info ~docv:"PROFILENAME" ["profile"; "p"]) $
    Term.const ()
  in
  let info = Cmd.info "log" in
  Cmd.v info term

let cmds =
  let confdir = XDGBaseDir.(default.config_home) in
  let confdir = Fpath.v confdir in
  let confdir = Fpath.add_seg confdir "opam-health-check" in
  let conffile = Fpath.add_seg confdir "config.yaml" in
  [
    init_cmd ~confdir ~conffile; (* TODO: Handle profilename on init *)
    add_user_cmd ~confdir ~conffile;
    add_ocaml_switch_cmd ~confdir ~conffile;
    set_ocaml_switch_cmd ~confdir ~conffile;
    rm_ocaml_switch_cmd ~confdir ~conffile;
    set_slack_webhooks_cmd ~confdir ~conffile;
    set_list_command_cmd ~confdir ~conffile;
    run_cmd ~confdir ~conffile;
    clear_cache_cmd ~confdir ~conffile;
    log_cmd ~confdir ~conffile;
    set_auto_run_interval_cmd ~confdir ~conffile;
    set_processes_cmd ~confdir ~conffile;
  ]

let () =
  let info =
    Cmd.info
      ~version:Config.version
      Config.name
  in
  exit @@ Cmd.eval (Cmd.group info cmds)
