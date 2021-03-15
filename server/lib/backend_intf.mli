type task = unit -> unit Lwt.t

module type S = sig
  type t

  val cache : Cache.t

  val get_log : t -> logdir:Server_workdirs.logdir -> comp:Intf.Compiler.t -> state:Intf.State.t -> pkg:string -> string Lwt.t

  val start : debug: bool -> Server_configfile.t -> Server_workdirs.t -> (t * task) Lwt.t
end
