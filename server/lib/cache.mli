module Opams_cache : Map.S with type key = string
module Revdeps_cache : Map.S with type key = string

type t

val create : unit -> t

val clear_and_init :
  t ->
  pkgs:(compilers:Intf.Compiler.t list -> Server_workdirs.logdir -> Intf.Pkg.t list Lwt.t) ->
  compilers:(Server_workdirs.logdir -> Intf.Compiler.t list Lwt.t) ->
  logdirs:(unit -> Server_workdirs.logdir list Lwt.t) ->
  opams:(unit -> OpamFile.OPAM.t Opams_cache.t Lwt.t) ->
  revdeps:(unit -> int Revdeps_cache.t Lwt.t) ->
  unit Lwt.t

val get_latest_logdir : t -> Server_workdirs.logdir option Lwt.t
val get_html : conf:Server_configfile.t -> t -> Html.query -> Server_workdirs.logdir -> string Lwt.t
val get_logdirs : t -> Server_workdirs.logdir list Lwt.t
val get_pkgs : logdir:Server_workdirs.logdir -> t -> Intf.Pkg.t list Lwt.t
val get_compilers : logdir:Server_workdirs.logdir -> t -> Intf.Compiler.t list Lwt.t
val get_opam : t -> string -> OpamFile.OPAM.t Lwt.t
val get_revdeps : t -> string -> int Lwt.t
val get_html_diff : conf:Server_configfile.t -> old_logdir:Server_workdirs.logdir -> new_logdir:Server_workdirs.logdir -> t -> string Lwt.t
val get_html_diff_list : t -> string Lwt.t
val get_html_run_list : t -> string Lwt.t
val get_json_latest_packages : t -> string Lwt.t
val get_json_run_packages : t -> Server_workdirs.logdir -> string Lwt.t