module State : sig
  type t = Good | Partial | Bad

  val equal : t -> t -> bool

  val from_string : string -> t
  val to_string : t -> string
end

module Compiler : sig
  type t

  val from_string : string -> t
  val to_string : t -> string

  val equal : t -> t -> bool
  val compare : t -> t -> int
end

module Instance : sig
  type t

  val create : Compiler.t -> State.t -> string Lwt.t -> t

  val compiler : t -> Compiler.t
  val state : t -> State.t
  val content : t -> string Lwt.t
end

module Pkg : sig
  type t

  val create :
    full_name:string ->
    instances:Instance.t list ->
    maintainers:string list ->
    t

  val equal : t -> t -> bool
  val compare : t -> t -> int

  val full_name : t -> string
  val name : t -> string
  val version : t -> string
  val maintainers : t -> string list
  val instances : t -> Instance.t list
end
