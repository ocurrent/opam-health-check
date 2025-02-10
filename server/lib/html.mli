type query = {
  available_compilers : Server_lib.Intf.Compiler.t list;
  compilers : Server_lib.Intf.Compiler.t list;
  show_available : Server_lib.Intf.Compiler.t list;
  show_only : Server_lib.Intf.State.t list;
  show_diff_only : bool;
  show_latest_only : bool;
  sort_by_revdeps : bool;
  maintainers : string * Re.re option;
  logsearch : string * (Re.re * Server_lib.Intf.Compiler.t) option;
}

val get_html :
  logdir:Server_lib.Workdirs.logdir ->
  conf:Server_lib.Configfile.t ->
  query ->
  Server_lib.Intf.Pkg.t list ->
  string

type diff = (Server_lib.Intf.Pkg_diff.t list * Server_lib.Intf.Pkg_diff.t list * Server_lib.Intf.Pkg_diff.t list * Server_lib.Intf.Pkg_diff.t list * Server_lib.Intf.Pkg_diff.t list)

val get_diff :
  old_logdir:Server_lib.Workdirs.logdir ->
  new_logdir:Server_lib.Workdirs.logdir ->
  conf:Server_lib.Configfile.t ->
  diff ->
  string

val get_diff_list :
  (Server_lib.Workdirs.logdir * Server_lib.Workdirs.logdir) list ->
  string

val get_run_list : Server_lib.Workdirs.logdir list -> string

val get_log : comp:Server_lib.Intf.Compiler.t -> pkg:string -> string -> string
