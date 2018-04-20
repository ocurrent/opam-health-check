type comp
type pkgs

val get_dirs : string -> comp list
val get_pkgs : logdir:string -> comp list -> pkgs
val get_html : comp list -> pkgs -> string

val comp_from_string : string -> comp
val comp_equal : comp -> comp -> bool
