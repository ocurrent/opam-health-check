type t
type profile

val init_with_values :
  confdir:Fpath.t ->
  hostname:string ->
  port:int ->
  username:string ->
  keyfile:Fpath.t ->
  Fpath.t ->
  unit

val init : confdir:Fpath.t -> Fpath.t -> unit
val from_file : confdir:Fpath.t -> Fpath.t -> t

val profile : profilename:string option -> t -> profile
val hostname : profile -> string
val port : profile -> int
val username : profile -> string
val keyfile : profile -> Fpath.t
