type t

val create : workdir:string -> t

val keysdir : t -> Fpath.t
val keyfile : username:string -> t -> Fpath.t

val logdir : t -> Fpath.t
val tmplogdir : t -> Fpath.t

val ilogdir : t -> Fpath.t
val switchilogdir : switch:string -> t -> Fpath.t
val ilogfile : switch:string -> t -> Fpath.t

val switchlogdir : switch:string -> t -> Fpath.t
val gooddir : switch:string -> t -> Fpath.t
val baddir : switch:string -> t -> Fpath.t

val tmpswitchlogdir : switch:string -> t -> Fpath.t
val tmplogfile : pkg:string -> switch:string -> t -> Fpath.t

val tmpgoodlog : pkg:string -> switch:string -> t -> Fpath.t
val tmpbadlog : pkg:string -> switch:string -> t -> Fpath.t

val configfile : t -> Fpath.t
val file_from_logdir : file:string -> t -> Fpath.t

val init_base : t -> unit Lwt.t
val init_base_job :
  switch:string ->
  stderr:Lwt_unix.file_descr ->
  t ->
  unit Lwt.t
