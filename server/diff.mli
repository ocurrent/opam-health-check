type query = {
  compilers : Backend.Pkg.comp list;
  show_available : Backend.Pkg.comp list;
  show_failures_only : bool;
  show_diff_only : bool;
  show_latest_only : bool;
  maintainers : string * Re.re;
}

val get_html : query -> (Backend.Pkg.pkg * Backend.Pkg.info) list -> string
