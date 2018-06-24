module type S = sig
  type t
  type task = unit -> unit Lwt.t

  val cache : Cache.t

  val get_log : t -> comp:Intf.Compiler.t -> state:Intf.State.t -> pkg:string -> string Lwt.t
  val get_compilers : t -> Intf.Compiler.t list Lwt.t
  val get_pkgs : t -> Intf.Pkg.t list Lwt.t

  val start : Server_configfile.t -> Server_workdirs.t -> (t * task) Lwt.t
end
