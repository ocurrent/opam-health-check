val clear_and_init : Backend.t -> unit

val get_html : Diff.query -> string Lwt.t
val get_compilers : unit -> Backend.Pkg.comp list Lwt.t
