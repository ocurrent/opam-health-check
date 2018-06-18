type query = {
  compilers : Pkg.comp list;
  show_available : Pkg.comp list;
  show_failures_only : bool;
  show_diff_only : bool;
  show_latest_only : bool;
  maintainers : string * Re.re;
}

val get_html : query -> (string * Pkg.info) list -> string
