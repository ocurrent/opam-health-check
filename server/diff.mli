type comp
type pkgs

type query = {
  compilers : comp list;
  show_available : comp list;
}

val default_query : Server_workdirs.t -> query Lwt.t

val get_dirs : Fpath.t -> comp list Lwt.t
val get_pkgs : Server_workdirs.t -> comp list -> pkgs Lwt.t
val get_html : query -> pkgs -> string

val comp_from_string : string -> comp
val comp_equal : comp -> comp -> bool
