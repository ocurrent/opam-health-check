module Maintainers_cache : Hashtbl.S with type key = string
module Revdeps_cache : Hashtbl.S with type key = string

type t

val create : unit -> t

val clear_and_init :
  t ->
  pkgs:(old:bool -> Server_workdirs.logdir -> Intf.Pkg.t list Lwt.t) ->
  compilers:(Server_workdirs.logdir -> Intf.Compiler.t list Lwt.t) ->
  logdirs:(unit -> Server_workdirs.logdir list Lwt.t) ->
  maintainers:(unit -> string list Maintainers_cache.t Lwt.t) ->
  revdeps:(unit -> int Revdeps_cache.t Lwt.t) ->
  html_diff:(unit -> string Lwt.t) ->
  unit

val get_html : conf:Server_configfile.t -> t -> Html.query -> string Lwt.t
val get_pkgs : old:bool -> t -> Intf.Pkg.t list Lwt.t
val get_compilers : old:bool -> t -> Intf.Compiler.t list Lwt.t
val get_maintainers : t -> string -> string list Lwt.t
val get_revdeps : t -> string -> int Lwt.t
val get_diff : t -> (Intf.Pkg_diff.t list * Intf.Pkg_diff.t list * Intf.Pkg_diff.t list * Intf.Pkg_diff.t list * Intf.Pkg_diff.t list) Lwt.t
val get_html_diff : t -> string Lwt.t
