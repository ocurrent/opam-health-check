open Lwt.Infix

let with_file_out ~flags file f =
  let flags = Unix.O_WRONLY::Unix.O_CREAT::Unix.O_TRUNC::Unix.O_NONBLOCK::flags in
  Lwt_io.with_file ~flags ~mode:Lwt_io.Output file f

let is_username_char = function
  | 'a'..'z' -> true
  | '-' | '_' -> true
  | _ -> false

let get_keyfile workdir username =
  (* TODO: Replace this test by Oca_lib.is_valid_filename ? *)
  if String.is_empty username || not (String.for_all is_username_char username) then
    failwith "Invalid username";
  Server_workdirs.keyfile ~username workdir

let create_userkey workdir username =
  let keyfile = get_keyfile workdir username in
  let key = Nocrypto.Rsa.generate 2048 in
  let key = Nocrypto.Rsa.sexp_of_priv key in
  let key = Sexplib.Sexp.to_string key in
  with_file_out ~flags:[Unix.O_EXCL] (Fpath.to_string keyfile) begin fun chan ->
    Lwt_io.write_line chan key
  end

let create_admin_key workdir =
  let username = Oca_lib.default_admin_name in
  let keyfile = get_keyfile workdir username in
  Lwt_unix.file_exists (Fpath.to_string keyfile) >>= function
  | true -> Lwt.return_unit
  | false -> create_userkey workdir username

let admin_action ~on_finished ~conf ~run_trigger workdir body =
  match String.split_on_char '\n' body with
  | "set-ocaml-switches"::switches ->
      let switches = List.map Intf.Compiler.from_string switches in
      let switches = List.sort Intf.Compiler.compare switches in
      Server_configfile.set_ocaml_switches conf switches
  | ["run"] ->
      Lwt_mvar.put run_trigger ()
  | ["add-user";username] ->
      create_userkey workdir username
  | ["clear-cache"] ->
      on_finished workdir;
      Lwt.return_unit
  | _ ->
      Lwt.fail_with "Action unrecognized."

let is_bzero = function
  | '\000' -> true
  | _ -> false

let get_user_key workdir user =
  let keyfile = get_keyfile workdir user in
  Lwt_io.with_file ~mode:Lwt_io.Input (Fpath.to_string keyfile) (Lwt_io.read ?count:None) >|= fun key ->
  Nocrypto.Rsa.priv_of_sexp (Sexplib.Sexp.of_string key)

let partial_decrypt key msg =
  Cstruct.to_string (Nocrypto.Rsa.decrypt ~key (Cstruct.of_string msg))

let rec decrypt key msg =
  let key_size = Nocrypto.Rsa.priv_bits key / 8 in
  if String.length msg <= key_size then
    String.drop_while is_bzero (partial_decrypt key msg)
  else
    let msg, next = String.take_drop key_size msg in
    partial_decrypt key msg ^ decrypt key next

let callback ~on_finished ~conf ~run_trigger workdir _conn _req body =
  Cohttp_lwt.Body.to_string body >>= fun body ->
  match String.Split.left ~by:"\n" body with
  | Some (pversion, body) when String.equal Oca_lib.protocol_version pversion ->
      begin match String.Split.left ~by:"\n" body with
      | Some (_, "") ->
          Lwt.fail_with "Empty message"
      | Some (user, body) ->
          get_user_key workdir user >>= fun key ->
          let body = decrypt key body in
          begin match String.Split.left ~by:"\n" body with
          | Some (user', body) when String.equal user user' ->
              admin_action ~on_finished ~conf ~run_trigger workdir body >>= fun () ->
              Cohttp_lwt_unix.Server.respond ~status:`OK ~body:`Empty ()
          | Some _ ->
              Lwt.fail_with "Identity couldn't be ensured"
          | None ->
              Lwt.fail_with "Identity check required"
          end
      | None ->
          Lwt.fail_with "Cannot find username"
      end
  | Some (pversion, _) ->
      Cohttp_lwt_unix.Server.respond_string
        ~status:`Upgrade_required
        ~body:("This server requires opam-check-all protocol version \
                '"^Oca_lib.protocol_version^"' but got '"^pversion^"'. \
                Please upgrade your client.")
        ()
  | None ->
      Lwt.fail_with "Cannot parse request"
