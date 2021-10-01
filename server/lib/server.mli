[@@@ocaml.warning "-67"] (* TODO: remove (requires OCaml >= 4.08) *)

module Make (Backend : Backend_intf.S) : sig
  val main : debug:bool -> cap_file:string -> workdir:string -> unit Lwt.t
end
