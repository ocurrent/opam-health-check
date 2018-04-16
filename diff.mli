type dir
type pkgs

val get_dirs : string -> dir list
val get_pkgs : dir list -> pkgs
val get_html : dir list -> pkgs -> string
