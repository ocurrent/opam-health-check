open Containers
open Lwt.Infix

let admin_action ~logdir user body =
  match String.split_on_char '\n' body with
  | "check"::dir::dockerfile ->
      let dockerfile = String.concat "\n" dockerfile in
      Lwt.ignore_result begin
        Check.get_pkgs ~dockerfile >>= fun (img_name, pkgs) ->
        Check.get_jobs ~img_name ~logdir pkgs
      end
  | _ ->
      assert false (* TODO *)

let is_bzero = function
  | '\000' -> true
  | _ -> false

(* TODO: https://github.com/c-cube/ocaml-containers/pull/214 *)
let ltrim s =
  let i = ref 0 in
  while !i < String.length s && is_bzero (String.unsafe_get s !i) do incr i done;
  if !i > 0 then String.sub s !i (String.length s - !i) else s

let is_username_char = function
  | 'a'..'z' -> true
  | '-' | '_' -> true
  | _ -> false

let get_user_key ~keysdir user =
  if not (String.is_empty user) && String.for_all is_username_char user then
    Lwt_io.with_file ~mode:Lwt_io.Input (Filename.concat keysdir user) Lwt_io.read >|= fun key ->
    X509.Encoding.Pem.Private_key.of_pem_cstruct1 (Cstruct.of_string key)
  else
    Lwt.fail_with "Invalid username"

let partial_decrypt key msg =
  Cstruct.to_string (Nocrypto.Rsa.decrypt ~key (Cstruct.of_string msg))

let rec decrypt key msg =
  let key_size = Nocrypto.Rsa.priv_bits key / 8 in
  if String.length msg <= key_size then
    ltrim (partial_decrypt key msg)
  else
    let msg, next = String.take_drop key_size msg in
    partial_decrypt key msg ^ decrypt key next

let callback ~logdir ~keysdir conn req body =
  Cohttp_lwt.Body.to_string body >>= fun body ->
  match String.Split.left ~by:"\n" body with
  | Some (user, "") ->
      Lwt.fail_with "Empty message"
  | Some (user, body) ->
      get_user_key ~keysdir user >>= fun (`RSA key) ->
      let body = decrypt key body in
      begin match String.Split.left ~by:"\n" body with
      | Some (user', body) when String.equal user user' ->
          admin_action ~logdir user body;
          Cohttp_lwt_unix.Server.respond ~status:`OK ~body:`Empty ()
      | Some (user', _) ->
          Lwt.fail_with "Identity couldn't be ensured"
      | None ->
          Lwt.fail_with "Identity check required"
      end
  | None ->
      Lwt.fail_with "Cannot find username"
