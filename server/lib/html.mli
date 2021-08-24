type query = {
  available_compilers : Intf.Compiler.t list;
  compilers : Intf.Compiler.t list;
  show_available : Intf.Compiler.t list;
  show_failures_only : bool;
  show_diff_only : bool;
  show_latest_only : bool;
  sort_by_revdeps : bool;
  maintainers : string * Re.re option;
  logsearch : string * (Re.re * Intf.Compiler.t) option;
}

val get_html :
  logdir:Server_workdirs.logdir ->
  query ->
  Intf.Pkg.t list ->
  string

type diff = (Intf.Pkg_diff.t list * Intf.Pkg_diff.t list * Intf.Pkg_diff.t list * Intf.Pkg_diff.t list * Intf.Pkg_diff.t list)

val get_diff :
  old_logdir:Server_workdirs.logdir ->
  new_logdir:Server_workdirs.logdir ->
  diff ->
  string

val get_diff_list :
  (Server_workdirs.logdir * Server_workdirs.logdir) list ->
  string

val get_run_list : Server_workdirs.logdir list -> string

val get_log : comp:Intf.Compiler.t -> pkg:string -> string -> string
