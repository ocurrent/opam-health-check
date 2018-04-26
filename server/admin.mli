val create_admin_key : keysdir:string -> unit Lwt.t

val callback :
  logdir:string ->
  keysdir:string ->
  Cohttp_lwt_unix.Server.conn ->
  Cohttp.Request.t ->
  Cohttp_lwt.Body.t ->
  (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t
