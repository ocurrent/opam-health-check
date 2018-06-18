val check :
  Server_workdirs.t ->
  on_finished:(Server_workdirs.t -> unit) ->
  dockerfile:string ->
  string ->
  unit Lwt.t
