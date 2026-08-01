open Lwt.Syntax

(* GitHub OAuth (web application flow) session handling for the log pages.

   Sessions and OAuth state parameters are self-contained HMAC-SHA256 tokens
   of the shape <expiry>.<mac>.<payload>, signed with a per-server key
   persisted in the work directory, so no session store is needed. *)

let cookie_name = "ohc_session"
let session_max_age = 30 * 24 * 60 * 60
let state_max_age = 10 * 60
let http_timeout = 15.0

let load_key workdir =
  let file = Fpath.to_string (Server_workdirs.keyfile ~username:"session" workdir) in
  Lwt.catch
    (fun () ->
      let* key = Lwt_io.with_file ~mode:Lwt_io.Input file (Lwt_io.read ?count:None) in
      if String.length key < 32 then Lwt.fail (Failure (file^" is too short"))
      else Lwt.return key)
    (function
    | Unix.Unix_error (err, _, _) when Stdlib.(=) err Unix.ENOENT ->
        let* key = Lwt_io.with_file ~mode:Lwt_io.Input "/dev/urandom" (fun ic -> Lwt_io.read ~count:32 ic) in
        if String.length key <> 32 then Lwt.fail (Failure "could not read /dev/urandom")
        else
          let+ () =
            Lwt_io.with_file ~mode:Lwt_io.Output ~perm:0o600 file
              (fun oc -> Lwt_io.write oc key)
          in
          key
    | otherwise -> Lwt.reraise otherwise)

let hmac ~key msg = Digestif.SHA256.(to_hex (hmac_string ~key msg))

let mac_equal mac mac' =
  (* both operands are hex of fixed length; compare in constant time *)
  String.length mac = String.length mac' &&
  Stdlib.(=) 0 (Seq.fold_left (fun acc (c, c') -> acc lor (Char.code c lxor Char.code c')) 0
                  (Seq.zip (String.to_seq mac) (String.to_seq mac')))

let make_token ~key ~kind ~max_age payload =
  let expiry = string_of_int (int_of_float (Unix.time ()) + max_age) in
  let mac = hmac ~key (kind^":"^expiry^":"^payload) in
  expiry^"."^mac^"."^payload

let verify_token ~key ~kind token =
  match String.split_on_char '.' token with
  | expiry :: mac :: payload ->
      let payload = String.concat "." payload in
      let valid =
        match int_of_string_opt expiry with
        | Some expiry -> expiry > int_of_float (Unix.time ())
        | None -> false
      in
      if valid && mac_equal mac (hmac ~key (kind^":"^expiry^":"^payload)) then
        Some payload
      else
        None
  | [] | [_] -> None

let make_state ~key path = make_token ~key ~kind:"state" ~max_age:state_max_age path
let verify_state ~key state = verify_token ~key ~kind:"state" state

let session_cookie ~key ~secure login =
  Printf.sprintf "%s=%s; Path=/; Max-Age=%d; HttpOnly; SameSite=Lax%s"
    cookie_name
    (make_token ~key ~kind:"session" ~max_age:session_max_age login)
    session_max_age
    (if secure then "; Secure" else "")

let verify_session ~key headers =
  let cookies = Cohttp.Cookie.Cookie_hdr.extract headers in
  match List.assoc_opt ~eq:String.equal cookie_name cookies with
  | None -> None
  | Some token -> verify_token ~key ~kind:"session" token

let authorize_url ~client_id ~redirect_uri ~state =
  Uri.to_string
    (Uri.with_query' (Uri.of_string "https://github.com/login/oauth/authorize")
       [("client_id", client_id); ("redirect_uri", redirect_uri); ("state", state)])

let http_request ?body ~meth ~headers url =
  let request () =
    let* v =
      Http_lwt_client.request
        ~config:(`HTTP_1_1 Httpaf.Config.default) (* TODO: Remove this when https://github.com/roburio/http-lwt-client/issues/7 is fixed *)
        ~meth
        ~headers:(("User-Agent", "opam-health-check") :: headers)
        ?body
        url
        (fun _resp acc x -> Lwt.return (acc ^ x)) ""
    in
    match v with
    | Ok ({Http_lwt_client.status = `OK; _}, body) -> Lwt.return (Ok body)
    | Ok (resp, body) -> Lwt.return (Error (Format.asprintf "%a: %s" Http_lwt_client.pp_response resp body))
    | Error (`Msg msg) -> Lwt.return (Error msg)
  in
  let timeout =
    let+ () = Lwt_unix.sleep http_timeout in
    Error (url^" timed out")
  in
  Lwt.pick [request (); timeout]

let json_string_field field body =
  match Yojson.Safe.from_string body with
  | exception _ -> None
  | json ->
      match Yojson.Safe.Util.member field json with
      | `String value -> Some value
      | _ -> None

(* Exchange the OAuth code for a token and return the GitHub login it
   belongs to. Any valid GitHub account is accepted: the point is to put a
   login wall in front of the log pages, not to restrict who reads them. *)
let login ~client_id ~client_secret ~redirect_uri ~code =
  let body =
    Uri.encoded_of_query
      [("client_id", [client_id]);
       ("client_secret", [client_secret]);
       ("redirect_uri", [redirect_uri]);
       ("code", [code])]
  in
  let* v =
    http_request ~meth:`POST
      ~headers:[("Accept", "application/json"); ("Content-Type", "application/x-www-form-urlencoded")]
      ~body
      "https://github.com/login/oauth/access_token"
  in
  match v with
  | Error e -> Lwt.return (Error e)
  | Ok body ->
      match json_string_field "access_token" body with
      | None -> Lwt.return (Error "GitHub did not return an access token")
      | Some token ->
          let* v =
            http_request ~meth:`GET
              ~headers:[("Accept", "application/json"); ("Authorization", "Bearer "^token)]
              "https://api.github.com/user"
          in
          match v with
          | Error e -> Lwt.return (Error e)
          | Ok body ->
              match json_string_field "login" body with
              | None -> Lwt.return (Error "GitHub did not return the user login")
              | Some login -> Lwt.return (Ok login)
