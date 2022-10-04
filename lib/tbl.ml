module type S = sig
  include CCHashtbl.S

  val add : unit
end

module Make (I : Stdlib.Hashtbl.HashedType) = struct
  include CCHashtbl.Make (I)

  let add = ()
end

let hash = Stdlib.Hashtbl.hash (* TODO: Remove this *)
