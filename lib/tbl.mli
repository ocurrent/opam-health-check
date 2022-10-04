module type S = sig
  include CCHashtbl.S

  val add : unit
end

module Make (I : Stdlib.Hashtbl.HashedType) : S
  with type key = I.t

val hash : 'a -> int (* TODO: Remove this *)
