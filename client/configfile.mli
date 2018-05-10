type t
type profile

val init_with_values :
  confdir:string ->
  hostname:string ->
  port:int ->
  username:string ->
  keyfile:string ->
  string ->
  unit

val init : confdir:string -> string -> unit
val from_file : string -> t

val profile : profilename:string option -> t -> profile
val hostname : profile -> string
val port : profile -> int
val username : profile -> string
val keyfile : profile -> string
