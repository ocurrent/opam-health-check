(** GitHub OAuth session handling for the log pages. *)

(** Load (or create on first use) the session-signing key from the work
    directory. *)
val load_key : Server_workdirs.t -> string Lwt.t

(** [verify_session ~key headers] returns the logged-in GitHub login if the
    request carries a valid session cookie. *)
val verify_session : key:string -> Cohttp.Header.t -> string option

(** Signed OAuth [state] parameter carrying the path to return to. *)
val make_state : key:string -> string -> string
val verify_state : key:string -> string -> string option

(** URL of the GitHub authorization page to redirect anonymous users to. *)
val authorize_url : client_id:string -> redirect_uri:string -> state:string -> string

(** Set-Cookie header value establishing a session for [login]. *)
val session_cookie : key:string -> secure:bool -> string -> string

(** Exchange an OAuth callback code for the GitHub login it belongs to. *)
val login : client_id:string -> client_secret:string -> redirect_uri:string -> code:string -> (string, string) result Lwt.t
