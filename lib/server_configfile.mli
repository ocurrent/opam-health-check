type t
type check

val from_workdir : Server_workdirs.t -> t

val port : t -> int
val admin_port : t -> int
val processes : t -> int
val checks : t -> check list

val name : check -> string
val priority : check -> int
val auto_run_interval : check -> int
val list_command : check -> string
val extra_command : check -> string option
val extra_env : check -> (string * string) list option
val ocaml_switches : check -> Intf.Switch.t list option
val slack_webhooks : check -> Uri.t list

val set_processes : t -> int -> unit Lwt.t

val set_priority : t -> check -> int -> unit Lwt.t
val set_auto_run_interval : t -> check -> int -> unit Lwt.t
val set_ocaml_switches : t -> check -> Intf.Switch.t list -> unit Lwt.t
val set_default_ocaml_switches : t -> check -> (unit -> Intf.Switch.t list Lwt.t) -> unit Lwt.t
val set_list_command : t -> check -> string -> unit Lwt.t
val set_extra_command : t -> check -> string option -> unit Lwt.t
val set_slack_webhooks : t -> check -> Uri.t list -> unit Lwt.t

val get_check : t -> check_name:string -> check
