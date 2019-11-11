type is_retry = bool

type action =
  | Manual of (Server_configfile.check * is_retry)
  | Regular of Server_configfile.check

val create_admin_key : Server_workdirs.t -> unit Lwt.t

val callback :
  on_finished:(Server_workdirs.t -> unit) ->
  conf:Server_configfile.t ->
  run_trigger:action Lwt_mvar.t ->
  Server_workdirs.t ->
  Cohttp_lwt_unix.Server.conn ->
  Cohttp.Request.t ->
  Cohttp_lwt.Body.t ->
  (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t
