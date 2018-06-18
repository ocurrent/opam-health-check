type state = Good | Partial | Bad
type comp = string

type info = {
  maintainers : string list;
  instances : (comp * state) list;
}

val state_eq : state -> state -> bool

val get_dirs : Fpath.t -> comp list Lwt.t
val fill_pkgs : update:(string -> ((comp * state) list -> (comp * state) list) -> unit) -> Server_workdirs.t -> unit Lwt.t

val comp_from_string : string -> comp
val comp_equal : comp -> comp -> bool
