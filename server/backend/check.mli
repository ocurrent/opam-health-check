val run :
  debug:bool ->
  cap_file:string ->
  on_finished:(Server_lib.Workdirs.t -> unit Lwt.t) ->
  conf:Server_lib.Configfile.t ->
  Oca_server.Cache.t ->
  Server_lib.Workdirs.t ->
  unit Lwt.t

val is_running : unit -> bool
val wait_current_run_to_finish : unit -> unit Lwt.t
