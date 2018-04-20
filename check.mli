type img
type pkgs

val get_pkgs : dockerfile:string -> (img * pkgs) Lwt.t

val get_jobs : img_name:img -> logdir:string -> pkgs -> unit Lwt.t
