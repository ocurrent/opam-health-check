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
  let key = Mirage_crypto_pk.Rsa.generate ~bits:2048 () in
  let key = Mirage_crypto_pk.Rsa.sexp_of_priv key in
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

let get_log workdir =
  let ilogdir = Server_workdirs.ilogdir workdir in
  Oca_lib.get_files ilogdir >>= fun logs ->
  let logs = List.sort String.compare logs in
  let logfile = Option.get_exn_or "no last log" (List.last_opt logs) in
  let logfile = Fpath.(ilogdir / logfile) in
  Lwt_unix.openfile (Fpath.to_string logfile) Unix.[O_RDONLY] 0o644 >>= fun fd ->
  let off = ref 0 in
  let rec loop () =
    let is_running = Check.is_running () in
    Lwt_unix.lseek fd 0 Unix.SEEK_END >>= fun new_off ->
    if new_off < 0 then
      assert false
    else if !off < new_off then begin
      Lwt_unix.lseek fd !off Unix.SEEK_SET >>= fun _ ->
      let len = new_off - !off in
      let buf = Bytes.create len in
      Lwt_unix.read fd buf 0 len >>= fun _ ->
      off := new_off;
      Lwt.return (Some (Bytes.to_string buf))
    end else if is_running then begin
      off := new_off;
      Lwt_unix.sleep 1. >>= fun () ->
      loop ()
    end else
      Lwt.return_none
  in
  Lwt.return loop

let admin_action ~on_finished ~conf ~run_trigger workdir body =
  begin match String.split_on_char '\n' body with
  | ["set-auto-run-interval"; i] ->
      Server_configfile.set_auto_run_interval conf (int_of_string i) >|= fun () ->
      (fun () -> Lwt.return_none)
  | ["set-processes"; i] ->
      let i = int_of_string i in
      if i < 0 then
        Lwt.fail_with "Cannot set the number of processes to a negative value."
      else
        Server_configfile.set_processes conf i >|= fun () ->
        (fun () -> Lwt.return_none)
  | ["add-ocaml-switch";name;switch] ->
      let switch = Intf.Switch.create ~name ~switch in
      let switches = switch :: Option.get_or ~default:[] (Server_configfile.ocaml_switches conf) in
      let switches = List.sort Intf.Switch.compare switches in
      Server_configfile.set_ocaml_switches conf switches >|= fun () ->
      (fun () -> Lwt.return_none)
  | ["set-ocaml-switch";name;switch] ->
      let switch = Intf.Switch.create ~name ~switch in
      let switches = Option.get_or ~default:[] (Server_configfile.ocaml_switches conf) in
      let idx, _ = Option.get_exn_or "can't find switch name" (List.find_idx (Intf.Switch.equal switch) switches) in
      let switches = List.set_at_idx idx switch switches in
      Server_configfile.set_ocaml_switches conf switches >|= fun () ->
      (fun () -> Lwt.return_none)
  | ["rm-ocaml-switch";name] ->
      let switch = Intf.Switch.create ~name ~switch:"(* TODO: remove this shit *)" in
      let switches = Option.get_or ~default:[] (Server_configfile.ocaml_switches conf) in
      let switches = List.remove ~eq:Intf.Switch.equal ~key:switch switches in
      Server_configfile.set_ocaml_switches conf switches >|= fun () ->
      (fun () -> Lwt.return_none)
  | "set-slack-webhooks"::webhooks ->
      let webhooks = List.map Uri.of_string webhooks in
      Server_configfile.set_slack_webhooks conf webhooks >|= fun () ->
      (fun () -> Lwt.return_none)
  | ["set-list-command";cmd] ->
      Server_configfile.set_list_command conf cmd >|= fun () ->
      (fun () -> Lwt.return_none)
  | ["run"] ->
      Lwt_mvar.put run_trigger () >|= fun () ->
      (fun () -> Lwt.return_none)
  | ["add-user";username] ->
      create_userkey workdir username >|= fun () ->
      (fun () -> Lwt.return_none)
  | ["clear-cache"] ->
      on_finished workdir;
      Lwt.return (fun () -> Lwt.return_none)
  | ["log"] ->
      get_log workdir
  | _ ->
      Lwt.fail_with "Action unrecognized."
  end >>= fun resp ->
  let stream = Lwt_stream.from resp in
  Cohttp_lwt_unix.Server.respond ~status:`OK ~body:(`Stream stream) ()

let is_bzero = function
  | '\000' -> true
  | _ -> false

let get_user_key workdir user =
  let keyfile = get_keyfile workdir user in
  Lwt_io.with_file ~mode:Lwt_io.Input (Fpath.to_string keyfile) (Lwt_io.read ?count:None) >|= fun key ->
  Mirage_crypto_pk.Rsa.priv_of_sexp (Sexplib.Sexp.of_string key)

let partial_decrypt key msg =
  Cstruct.to_string (Mirage_crypto_pk.Rsa.decrypt ~key (Cstruct.of_string msg))

let rec decrypt key msg =
  let key_size = Mirage_crypto_pk.Rsa.priv_bits key / 8 in
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
              admin_action ~on_finished ~conf ~run_trigger workdir body
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
        ~body:("This server requires opam-health-check protocol version \
                '"^Oca_lib.protocol_version^"' but got '"^pversion^"'. \
                Please upgrade your client.")
        ()
  | None ->
      Lwt.fail_with "Cannot parse request"
