type t

val create : workdir:string -> t

val keysdir : t -> Fpath.t
val keyfile : username:string -> t -> Fpath.t

val logdir : t -> Fpath.t
val tmplogdir : t -> Fpath.t

val ilogdir : t -> Fpath.t
val ilogfile : t -> Fpath.t

val switchlogdir : switch:Intf.Compiler.t -> t -> Fpath.t
val gooddir : switch:Intf.Compiler.t -> t -> Fpath.t
val partialdir : switch:Intf.Compiler.t -> t -> Fpath.t
val baddir : switch:Intf.Compiler.t -> t -> Fpath.t

val tmpswitchlogdir : switch:Intf.Compiler.t -> t -> Fpath.t
val tmplogfile : pkg:string -> switch:Intf.Compiler.t -> t -> Fpath.t

val tmpgoodlog : pkg:string -> switch:Intf.Compiler.t -> t -> Fpath.t
val tmppartiallog : pkg:string -> switch:Intf.Compiler.t -> t -> Fpath.t
val tmpbadlog : pkg:string -> switch:Intf.Compiler.t -> t -> Fpath.t

val maintainersdir : t -> Fpath.t
val maintainersfile : pkg:string -> t -> Fpath.t

val tmpmaintainersdir : t -> Fpath.t
val tmpmaintainersfile : pkg:string -> t -> Fpath.t

val configfile : t -> Fpath.t
val file_from_logdir : file:string -> t -> Fpath.t

val init_base : t -> unit Lwt.t
val init_base_jobs : stderr:Lwt_unix.file_descr -> t -> unit Lwt.t
val init_base_job : switch:Intf.Compiler.t -> t -> unit Lwt.t
