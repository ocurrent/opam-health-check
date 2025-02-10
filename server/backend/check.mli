val run :
  debug:bool ->
  cap_file:string ->
  on_finished:(Server_lib.Server_workdirs.t -> unit Lwt.t) ->
  conf:Server_lib.Server_configfile.t ->
  Oca_server.Cache.t ->
  Server_lib.Server_workdirs.t ->
  unit Lwt.t

val is_running : unit -> bool
val wait_current_run_to_finish : unit -> unit Lwt.t
