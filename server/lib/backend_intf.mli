module type CACHE = sig
  type backend

  val clear_and_init : backend -> unit

  val get_html : Html.query -> string Lwt.t
  val get_compilers : unit -> Intf.Compiler.t list Lwt.t
  val get_pkgsinfo : unit -> Obi.Index.pkg list Lwt.t
end

module type BACKEND = sig
  type t

  val get_log : t -> comp:Intf.Compiler.t -> state:Intf.State.t -> pkg:string -> string Lwt.t
  val get_compilers : t -> Intf.Compiler.t list Lwt.t
  val get_pkgs : t -> Obi.Index.pkg list Lwt.t -> Intf.Compiler.t list Lwt.t -> Intf.Pkg.t list Lwt.t
end

module type S = sig
  type t
  type task = unit -> unit Lwt.t

  module Cache : CACHE with type backend = t

  include BACKEND with type t := t

  val start : Server_configfile.t -> Server_workdirs.t -> (t * task) Lwt.t
end
