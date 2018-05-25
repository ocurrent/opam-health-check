type t

val create : workdir:string -> t

val keysdir : t -> string
val keyfile : username:string -> t -> string

val logdir : t -> string
val tmplogdir : t -> string

val ilogdir : t -> string
val switchilogdir : switch:string -> t -> string
val ilogfile : switch:string -> time:float -> t -> string

val switchlogdir : switch:string -> t -> string
val gooddir : switch:string -> t -> string
val baddir : switch:string -> t -> string
val logfile : pkg:string -> switch:string -> t -> string

val tmpswitchlogdir : switch:string -> t -> string
val tmpgooddir : switch:string -> t -> string
val tmpbaddir : switch:string -> t -> string

val tmpgoodlog : pkg:string -> switch:string -> t -> string
val tmpbadlog : pkg:string -> switch:string -> t -> string

val configfile : t -> string
val file_from_logdir : file:string -> t -> string

val init_base : t -> unit Lwt.t
val init_base_job : switch:string -> t -> unit Lwt.t
