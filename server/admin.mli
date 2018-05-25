val create_admin_key : Server_workdirs.t -> unit Lwt.t

val callback :
  Server_workdirs.t ->
  Cohttp_lwt_unix.Server.conn ->
  Cohttp.Request.t ->
  Cohttp_lwt.Body.t ->
  (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t
