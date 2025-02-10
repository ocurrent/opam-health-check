module Opams_cache : Map.S with type key = string
module Revdeps_cache : Map.S with type key = string

type t

val create : unit -> t

val clear_and_init :
  t ->
  pkgs:(compilers:Server_lib.Intf.Compiler.t list -> Server_lib.Workdirs.logdir -> Server_lib.Intf.Pkg.t list Lwt.t) ->
  compilers:(Server_lib.Workdirs.logdir -> Server_lib.Intf.Compiler.t list Lwt.t) ->
  logdirs:(unit -> Server_lib.Workdirs.logdir list Lwt.t) ->
  opams:(unit -> OpamFile.OPAM.t Opams_cache.t Lwt.t) ->
  revdeps:(unit -> int Revdeps_cache.t Lwt.t) ->
  unit Lwt.t

val get_latest_logdir : t -> Server_lib.Workdirs.logdir option Lwt.t
val get_html : conf:Server_lib.Configfile.t -> t -> Html.query -> Server_lib.Workdirs.logdir -> string Lwt.t
val get_logdirs : t -> Server_lib.Workdirs.logdir list Lwt.t
val get_pkgs : logdir:Server_lib.Workdirs.logdir -> t -> Server_lib.Intf.Pkg.t list Lwt.t
val get_compilers : logdir:Server_lib.Workdirs.logdir -> t -> Server_lib.Intf.Compiler.t list Lwt.t
val get_opam : t -> string -> OpamFile.OPAM.t Lwt.t
val get_revdeps : t -> string -> int Lwt.t
val get_html_diff : conf:Server_lib.Configfile.t -> old_logdir:Server_lib.Workdirs.logdir -> new_logdir:Server_lib.Workdirs.logdir -> t -> string Lwt.t
val get_html_diff_list : t -> string Lwt.t
val get_html_run_list : t -> string Lwt.t
val get_json_latest_packages : t -> string Lwt.t
