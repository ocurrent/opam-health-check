type query = {
  available_compilers : Backend.Intf.Compiler.t list;
  compilers : Backend.Intf.Compiler.t list;
  show_available : Backend.Intf.Compiler.t list;
  show_failures_only : bool;
  show_diff_only : bool;
  show_latest_only : bool;
  maintainers : string * Re.re;
}

val get_html : query -> Backend.Intf.Pkg.t list -> string
