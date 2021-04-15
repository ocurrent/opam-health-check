module Opams_cache : Hashtbl.S with type key = string
module Revdeps_cache : Hashtbl.S with type key = string

type t

val create : unit -> t

val clear_and_init :
  t ->
  pkgs:(compilers:Intf.Compiler.t list -> Server_workdirs.logdir -> Intf.Pkg.t list Lwt.t) ->
  compilers:(Server_workdirs.logdir -> Intf.Compiler.t list Lwt.t) ->
  logdirs:(unit -> Server_workdirs.logdir list Lwt.t) ->
  opams:(unit -> OpamFile.OPAM.t Opams_cache.t Lwt.t) ->
  revdeps:(unit -> int Revdeps_cache.t Lwt.t) ->
  unit

val get_latest_logdir : t -> Server_workdirs.logdir Lwt.t
val get_html : t -> Html.query -> Server_workdirs.logdir -> string Lwt.t
val get_logdirs : t -> Server_workdirs.logdir list Lwt.t
val get_pkgs : logdir:Server_workdirs.logdir -> t -> Intf.Pkg.t list Lwt.t
val get_compilers : logdir:Server_workdirs.logdir -> t -> Intf.Compiler.t list Lwt.t
val get_opam : t -> string -> OpamFile.OPAM.t Lwt.t
val get_revdeps : t -> string -> int Lwt.t
val get_html_diff : old_logdir:Server_workdirs.logdir -> new_logdir:Server_workdirs.logdir -> t -> string Lwt.t
val get_html_diff_list : t -> string Lwt.t
val get_html_run_list : t -> string Lwt.t
