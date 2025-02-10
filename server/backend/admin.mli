val create_admin_key : Server_lib.Server_workdirs.t -> unit Lwt.t

val callback :
  on_finished:(Server_lib.Server_workdirs.t -> unit Lwt.t) ->
  conf:Server_lib.Server_configfile.t ->
  run_trigger:unit Lwt_mvar.t ->
  Server_lib.Server_workdirs.t ->
  Cohttp_lwt_unix.Server.conn ->
  Cohttp.Request.t ->
  Cohttp_lwt.Body.t ->
  (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t
