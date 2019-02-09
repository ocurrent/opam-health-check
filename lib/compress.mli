type t

val compress : ?level:int -> string -> t
val uncompress : t -> string
