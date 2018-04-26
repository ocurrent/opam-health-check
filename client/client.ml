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

let send_msg ~key ~username ~hostname msg =
  let key = parse_key key in
  let prefix = username^"\n" in
  let msg = encrypt_msg ~key (prefix^msg) in
  let uri = Uri.make ~scheme:"http" ~host:hostname ~port:9999 () in
  Lwt_main.run (Cohttp_lwt_unix.Client.post ~body:(`String (prefix^msg)) uri)

(* TODO: Have a config file to store repetitive informations *)
let () =
  match Sys.argv with
  | [|_; key; username; hostname; comp; dockerfile|] ->
      print_endline "Sending command...";
      let dockerfile = IO.with_in dockerfile IO.read_all in
      let msg = "check\n"^comp^"\n"^dockerfile in
      let res, body = send_msg ~key ~username ~hostname msg in
      begin match Cohttp.Response.status res with
      | `OK -> print_endline "Command sent successfully"
      | _ -> prerr_endline "A problem occured"; exit 1
      end
  | _ ->
      prerr_endline "Read the code and try again";
      exit 1
