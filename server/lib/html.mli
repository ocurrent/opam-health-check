type query = {
  available_compilers : Intf.Compiler.t list;
  compilers : Intf.Compiler.t list;
  show_available : Intf.Compiler.t list;
  show_failures_only : bool;
  show_diff_only : bool;
  show_latest_only : bool;
  maintainers : string * Re.re;
}

val get_html : query -> Intf.Pkg.t list -> string
