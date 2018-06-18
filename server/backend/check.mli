val check :
  Server_workdirs.t ->
  on_finished:(unit -> unit) ->
  dockerfile:string ->
  string ->
  unit Lwt.t
