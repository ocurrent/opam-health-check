type task = unit -> unit Lwt.t

module type S = sig
  type t

  val cache : Cache.t

  val get_log : t -> logdir:Server_workdirs.logdir -> comp:Intf.Compiler.t -> state:Intf.State.t -> pkg:string -> string option Lwt.t

  val start : debug: bool -> cap_file:string -> Server_configfile.t -> Server_workdirs.t -> (t * unit Lwt.t * task) Lwt.t
end
