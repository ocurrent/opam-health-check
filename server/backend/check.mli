val run :
  on_finished:(Server_workdirs.t -> unit) ->
  conf:Server_configfile.t ->
  Server_workdirs.t ->
  unit Lwt.t
