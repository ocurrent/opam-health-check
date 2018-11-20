type t

val from_workdir : Server_workdirs.t -> t

val port : t -> int
val admin_port : t -> int
val list_command : t -> string
