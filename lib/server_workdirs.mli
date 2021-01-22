type t

val create : cwd:string -> workdir:string -> t

val keysdir : t -> Fpath.t
val keyfile : username:string -> t -> Fpath.t

type logdir

val new_logdir : compressed:bool -> hash:string -> start_time:float -> t -> logdir
val logdirs : t -> logdir list Lwt.t

val logdir_equal : logdir -> logdir -> bool
val get_logdir_name : logdir -> string
val get_logdir_hash : logdir -> string
val get_logdir_time : logdir -> float

val goodfiles : switch:Intf.Compiler.t -> logdir -> string list Lwt.t
val partialfiles : switch:Intf.Compiler.t -> logdir -> string list Lwt.t
val badfiles : switch:Intf.Compiler.t -> logdir -> string list Lwt.t
val notavailablefiles : switch:Intf.Compiler.t -> logdir -> string list Lwt.t
val internalfailurefiles : switch:Intf.Compiler.t -> logdir -> string list Lwt.t
val logdir_get_content : comp:Intf.Compiler.t -> state:Intf.State.t -> pkg:string -> logdir -> string Lwt.t
val logdir_get_compilers : logdir -> Intf.Compiler.t list Lwt.t
val logdir_move : switches:Intf.Compiler.t list -> logdir -> unit Lwt.t
val logdir_search : switch:string -> regexp:string -> logdir -> string list Lwt.t

val ilogdir : t -> Fpath.t
val new_ilogfile : start_time:float -> t -> Fpath.t

val tmplogdir : logdir -> Fpath.t
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

val init_base : t -> unit Lwt.t
val init_base_jobs : switches:Intf.Switch.t list -> logdir -> unit Lwt.t
