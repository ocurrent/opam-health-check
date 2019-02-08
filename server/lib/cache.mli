module Maintainers_cache : Hashtbl.S with type key = string

type t

val create : unit -> t

val clear_and_init :
  t ->
  (unit -> Intf.Pkg.t list Lwt.t) ->
  (unit -> Intf.Compiler.t list Lwt.t) ->
  (unit -> string list Maintainers_cache.t Lwt.t) ->
  (unit -> string Lwt.t) ->
  unit

val get_html : conf:Server_configfile.t -> t -> Html.query -> string Lwt.t
val get_pkgs : t -> Intf.Pkg.t list Lwt.t
val get_compilers : t -> Intf.Compiler.t list Lwt.t
val get_maintainers : t -> string -> string list Lwt.t
val get_diff : t -> Intf.Pkg_diff.t list Lwt.t
val get_html_diff : t -> string Lwt.t
