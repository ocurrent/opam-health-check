type comp
type state

type info = {
  maintainers : string list;
  instances : (comp * state) list;
}

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
val fill_pkgs : update:(string -> ((comp * state) list -> (comp * state) list) -> unit) -> Server_workdirs.t -> unit Lwt.t
val get_html : query -> (string * info) list -> string

val comp_from_string : string -> comp
val comp_equal : comp -> comp -> bool
