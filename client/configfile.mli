type t
type profile

val init : confdir:string -> string -> unit
val from_file : string -> t

val profile : profilename:string option -> t -> profile
val hostname : profile -> string
val port : profile -> int
val username : profile -> string
val keyfile : profile -> string
