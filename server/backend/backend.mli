type t
type task = unit -> unit Lwt.t

val get_log : t -> comp:Intf.Compiler.t -> state:Intf.State.t -> pkg:string -> string Lwt.t
val get_compilers : t -> Intf.Compiler.t list Lwt.t
val get_pkgs : t -> Obi.Index.pkg list Lwt.t -> Intf.Compiler.t list Lwt.t -> Intf.Pkg.t list Lwt.t

val start : on_finished:(t -> unit) -> (unit -> Obi.Index.pkg list Lwt.t) -> (t * task) Lwt.t
