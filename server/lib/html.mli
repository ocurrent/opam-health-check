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

val get_html : conf:Server_configfile.t -> query -> Intf.Pkg.t list -> string Lwt.t
val get_diff : conf:Server_configfile.t -> (Intf.Pkg_diff.t list * Intf.Pkg_diff.t list * Intf.Pkg_diff.t list * Intf.Pkg_diff.t list * Intf.Pkg_diff.t list) -> string
