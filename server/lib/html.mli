type query = {
  available_compilers : Intf.Compiler.t list;
  compilers : Intf.Compiler.t list;
  show_available : Intf.Compiler.t list;
  show_failures_only : bool;
  show_diff_only : bool;
  show_latest_only : bool;
  maintainers : string * Re.re;
  logsearch : string * Re.re;
  logsearch_comp : Intf.Compiler.t;
}

val get_html : conf:Server_configfile.t -> query -> Intf.Pkg.t list -> string Lwt.t
val get_diff : Intf.Pkg_diff.t list -> string
