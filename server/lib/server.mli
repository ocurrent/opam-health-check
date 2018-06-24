module Make (Backend : Backend_intf.S) : sig
  val main : workdir:string -> unit Lwt.t
end
