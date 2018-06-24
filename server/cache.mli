val clear_and_init : Backend.t -> unit

val get_html : Html.query -> string Lwt.t
val get_compilers : unit -> Intf.Compiler.t list Lwt.t
