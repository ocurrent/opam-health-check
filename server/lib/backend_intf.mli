type task = unit -> unit Lwt.t

module type S = sig
  type t

  val cache : Cache.t

  val get_log : t -> logdir:Server_lib.Workdirs.logdir -> comp:Server_lib.Intf.Compiler.t -> state:Server_lib.Intf.State.t -> pkg:string -> string option Lwt.t

  val start : debug: bool -> cap_file:string -> Server_lib.Configfile.t -> Server_lib.Workdirs.t -> (t * task) Lwt.t
end
