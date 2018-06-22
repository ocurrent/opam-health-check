module Intf : sig
  module State : sig
    type t = Good | Partial | Bad

    val equal : t -> t -> bool
  end

  module Compiler : sig
    type t

    val from_string : string -> t
    val to_string : t -> string

    val equal : t -> t -> bool
  end

  module Instance : sig
    type t

    val compiler : t -> Compiler.t
    val state : t -> State.t
  end

  module Pkg : sig
    type t

    val equal : t -> t -> bool
    val compare : t -> t -> int

    val full_name : t -> string
    val name : t -> string
    val maintainers : t -> string list
    val instances : t -> Instance.t list
  end
end

type t
type task = unit -> unit Lwt.t

val get_compilers : t -> Intf.Compiler.t list Lwt.t
val get_pkgs : Obi.Index.pkg list Lwt.t -> Intf.Compiler.t list Lwt.t -> t -> Intf.Pkg.t list Lwt.t

val start : on_finished:(t -> unit) -> Server_configfile.t -> Server_workdirs.t -> (t * task) Lwt.t
