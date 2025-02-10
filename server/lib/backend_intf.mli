type task = unit -> unit Lwt.t

module type S = sig
  type t

  val cache : Cache.t

  val get_log : t -> logdir:Server_lib.Server_workdirs.logdir -> comp:Server_lib.Intf.Compiler.t -> state:Server_lib.Intf.State.t -> pkg:string -> string option Lwt.t

  val start : debug: bool -> cap_file:string -> Server_lib.Server_configfile.t -> Server_lib.Server_workdirs.t -> (t * task) Lwt.t
end
