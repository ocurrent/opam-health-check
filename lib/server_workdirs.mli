type t

val create : workdir:string -> t

val keysdir : t -> Fpath.t
val keyfile : username:string -> t -> Fpath.t

type logdir

val new_logdir : hash:string -> start_time:float -> t -> logdir
val logdirs : t -> logdir list Lwt.t
val tmplogdir : logdir -> Fpath.t

val logdir_from_string : t -> string -> logdir
val logdir_equal : logdir -> logdir -> bool
val get_logdir_name : logdir -> string
val get_logdir_path : logdir -> Fpath.t
val get_logdir_hash : logdir -> string
val get_logdir_time : logdir -> float

val ilogdir : t -> Fpath.t
val new_ilogfile : start_time:float -> t -> Fpath.t

val switchlogdir : switch:Intf.Compiler.t -> logdir -> Fpath.t
val gooddir : switch:Intf.Compiler.t -> logdir -> Fpath.t
val partialdir : switch:Intf.Compiler.t -> logdir -> Fpath.t
val baddir : switch:Intf.Compiler.t -> logdir -> Fpath.t
val notavailabledir : switch:Intf.Compiler.t -> logdir -> Fpath.t
val internalfailuredir : switch:Intf.Compiler.t -> logdir -> Fpath.t

val tmpswitchlogdir : switch:Intf.Compiler.t -> logdir -> Fpath.t
val tmplogfile : pkg:string -> switch:Intf.Compiler.t -> logdir -> Fpath.t

val tmpgoodlog : pkg:string -> switch:Intf.Compiler.t -> logdir -> Fpath.t
val tmppartiallog : pkg:string -> switch:Intf.Compiler.t -> logdir -> Fpath.t
val tmpbadlog : pkg:string -> switch:Intf.Compiler.t -> logdir -> Fpath.t
val tmpnotavailablelog : pkg:string -> switch:Intf.Compiler.t -> logdir -> Fpath.t
val tmpinternalfailurelog : pkg:string -> switch:Intf.Compiler.t -> logdir -> Fpath.t

val metadatadir : t -> Fpath.t
val maintainersdir : t -> Fpath.t
val maintainersfile : pkg:string -> t -> Fpath.t
val revdepsdir : t -> Fpath.t
val revdepsfile : pkg:string -> t -> Fpath.t

val tmpmetadatadir : logdir -> Fpath.t
val tmpmaintainersdir : logdir -> Fpath.t
val tmpmaintainersfile : pkg:string -> logdir -> Fpath.t
val tmprevdepsdir : logdir -> Fpath.t
val tmprevdepsfile : pkg:string -> logdir -> Fpath.t

val configfile : t -> Fpath.t
val file_from_logdir : file:string -> logdir -> Fpath.t

val init_base : t -> unit Lwt.t
val init_base_jobs : switches:Intf.Switch.t list -> logdir -> unit Lwt.t
