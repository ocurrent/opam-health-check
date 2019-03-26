val run :
  on_finished:(Server_workdirs.t -> unit) ->
  is_retry:bool ->
  conf:Server_configfile.t ->
  Server_workdirs.t ->
  unit Lwt.t

val is_running : unit -> bool
