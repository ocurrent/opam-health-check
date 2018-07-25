val check :
  Server_workdirs.t ->
  no_cache:bool ->
  on_finished:(Server_workdirs.t -> unit) ->
  dockerfile:string ->
  Intf.Compiler.t ->
  unit Lwt.t
