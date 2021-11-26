module Make (_ : Backend_intf.S) : sig
  val main : debug:bool -> cap_file:string -> workdir:string -> unit Lwt.t
end
