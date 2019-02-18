type task = unit -> unit Lwt.t

module type S = sig
  type t

  val cache : Cache.t

  val get_log : t -> old:bool -> comp:Intf.Compiler.t -> state:Intf.State.t -> pkg:string -> string Lwt.t
  val get_compilers : old:bool -> t -> Intf.Compiler.t list Lwt.t
  val get_pkgs : old:bool -> t -> Intf.Pkg.t list Lwt.t

  val start : Server_configfile.t -> Server_workdirs.t -> (t * task) Lwt.t
end
