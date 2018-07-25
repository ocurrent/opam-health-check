val check :
  Server_workdirs.t ->
  no_cache:bool ->
  on_finished:(Server_workdirs.t -> unit) ->
  dockerfile:string ->
  string ->
  unit Lwt.t
