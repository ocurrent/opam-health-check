type t

val from_workdir : string -> t

val port : t -> int
val admin_port : t -> int
