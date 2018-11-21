type t

val create : unit -> t
val clear_and_init : t -> (unit -> Intf.Pkg.t list Lwt.t) -> (unit -> Intf.Compiler.t list Lwt.t) -> unit

val get_html : conf:Server_configfile.t -> t -> Html.query -> string Lwt.t
val get_compilers : t -> Intf.Compiler.t list Lwt.t
val get_pkgsinfo : t -> Obi.Index.pkg list Lwt.t
