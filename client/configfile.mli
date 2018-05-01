type t
type profile

val from_file : string -> t option

val profile : profilename:string option -> t -> profile
val hostname : profile -> string
val port : profile -> int
val username : profile -> string
val keyfile : profile -> string
