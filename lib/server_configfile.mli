type t

val from_workdir : Server_workdirs.t -> t

val port : t -> int
val admin_port : t -> int
val list_command : t -> string
val ocaml_switches : t -> Intf.Compiler.t list

val set_ocaml_switches : t -> Intf.Compiler.t list -> unit Lwt.t
