val set_ocaml_switches : Intf.Compiler.t list -> unit Lwt.t

val run :
  on_finished:(Server_workdirs.t -> unit) ->
  conf:Server_configfile.t ->
  Server_workdirs.t ->
  unit Lwt.t
