type comp
type pkgs

type query = {
  compilers : comp list;
  show_available : comp list;
  show_failures_only : bool;
  show_diff_only : bool;
  show_latest_only : bool;
  maintainers : string * Re.re;
}

val default_query : Server_workdirs.t -> query Lwt.t

val get_dirs : Fpath.t -> comp list Lwt.t
val get_pkgs : Server_workdirs.t -> comp list -> pkgs Lwt.t
val get_html : get_pkginfo:(string -> Metainfo.t) -> query -> pkgs -> string

val comp_from_string : string -> comp
val comp_equal : comp -> comp -> bool
