type task = unit -> unit Lwt.t

module type S = sig
  type t

  val cache : Cache.t

  val get_log : t -> logdir:Server_workdirs.logdir -> comp:Intf.Compiler.t -> state:Intf.State.t -> pkg:string -> string Lwt.t
  val get_compilers : Server_workdirs.logdir -> Intf.Compiler.t list Lwt.t
  val get_pkgs : old:bool -> Server_workdirs.logdir -> Intf.Pkg.t list Lwt.t

  val start : Server_configfile.t -> Server_workdirs.t -> (t * task) Lwt.t
end
