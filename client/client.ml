open Containers

let parse_key key =
  let key = IO.with_in key IO.read_all in
  let key = Nocrypto.Rsa.priv_of_sexp (Sexplib.Sexp.of_string key) in
  Nocrypto.Rsa.pub_of_priv key

let partial_encrypt key msg =
  Cstruct.to_string (Nocrypto.Rsa.encrypt ~key (Cstruct.of_string msg))

let rec encrypt_msg ~key msg =
  let max_size = Nocrypto.Rsa.pub_bits key / 8 in
  if String.length msg <= max_size then
    partial_encrypt key msg
  else
    let msg, next = String.take_drop max_size msg in
    partial_encrypt key msg ^ encrypt_msg ~key next

let send_msg ~key ~username ~hostname ~port msg =
  let key = parse_key key in
  let prefix = username^"\n" in
  let msg = encrypt_msg ~key (prefix^msg) in
  let uri = Uri.make ~scheme:"http" ~host:hostname ~port () in
  let prefix = Oca_lib.protocol_version^"\n"^prefix in
  Lwt_main.run (Cohttp_lwt_unix.Client.post ~body:(`String (prefix^msg)) uri)

let check ~confdir ~conffile comp dockerfile =
  (* TODO: Catch the exception if the config file doesn't exist *)
  let conf = Configfile.from_file conffile in
  let conf = Configfile.profile ~profilename:None conf in
  let hostname = Configfile.hostname conf in
  let port = Configfile.port conf in
  let username = Configfile.username conf in
  let keyfile = Configfile.keyfile conf in
  let key = Filename.concat confdir keyfile in
  print_endline "Sending command...";
  let dockerfile = IO.with_in dockerfile IO.read_all in
  let msg = "check\n"^comp^"\n"^dockerfile in
  let res, body = send_msg ~key ~username ~hostname ~port msg in
  match Cohttp.Response.status res with
  | `OK -> print_endline "Command sent successfully"
  | _ -> prerr_endline "A problem occured"; raise Exit

let check_cmd ~confdir ~conffile =
  let term =
    let ($) = Cmdliner.Term.($) in
    Cmdliner.Term.const (check ~confdir ~conffile) $
    Cmdliner.Arg.(required & pos 0 (some string) None & info ~docv:"NAME" []) $
    Cmdliner.Arg.(required & pos 1 (some string) None & info ~docv:"DOCKERFILE" [])
  in
  let info = Cmdliner.Term.info "check" in
  (term, info)

let init ~confdir ~conffile () =
  Configfile.init ~confdir conffile

let init_cmd ~confdir ~conffile =
  let term =
    let ($) = Cmdliner.Term.($) in
    Cmdliner.Term.const (init ~confdir ~conffile) $
    Cmdliner.Term.const ()
  in
  let info = Cmdliner.Term.info "init" in
  (term, info)

let cmds =
  let confdir = XDGBaseDir.(default.config_home) in
  let confdir = Filename.concat confdir "opam-check-all" in
  let conffile = Filename.concat confdir "config.yaml" in
  [
    init_cmd ~confdir ~conffile;
    check_cmd ~confdir ~conffile;
  ]

let () =
  let term = Cmdliner.Term.const () in
  let info =
    Cmdliner.Term.info
      ~version:Config.version
      Config.name
  in
  let eval = Cmdliner.Term.eval_choice ~catch:false in
  try Cmdliner.Term.exit (eval (term, info) cmds) with
  | Exit -> exit 1
