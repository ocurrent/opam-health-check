type state = Good | Partial | Bad
type comp
type instance = (comp * state)
type pkg

type info = {
  maintainers : string list;
  instances : instance list;
}

val state_eq : state -> state -> bool

val pkg_equal : pkg -> pkg -> bool
val pkg_compare : pkg -> pkg -> int
val pkg_to_string : pkg -> string
val pkg_name_to_string : pkg -> string

val get_compilers : Server_workdirs.t -> comp list Lwt.t
val fill_pkgs : update:(pkg -> (instance list -> instance list) -> unit) -> Server_workdirs.t -> unit Lwt.t

val comp_from_string : string -> comp
val comp_to_string : comp -> string
val comp_equal : comp -> comp -> bool
