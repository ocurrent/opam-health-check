type t

val from_workdir : Server_workdirs.t -> t

val name : t -> string
val port : t -> int
val public_url : t -> string
val admin_port : t -> int
val auto_run_interval : t -> int
val processes : t -> int
val enable_dune_cache : t -> bool
val enable_in_memory_logs : t -> bool
val enable_opam_alpha_repository : t -> bool
val with_test : t -> bool
val list_command : t -> string
val extra_command : t -> string option
val ocaml_switches : t -> Intf.Switch.t list option
val slack_webhooks : t -> Uri.t list

val set_auto_run_interval : t -> int -> unit Lwt.t
val set_processes : t -> int -> unit Lwt.t
val set_ocaml_switches : t -> Intf.Switch.t list -> unit Lwt.t
val set_default_ocaml_switches : t -> (unit -> Intf.Switch.t list Lwt.t) -> unit Lwt.t
val set_list_command : t -> string -> unit Lwt.t
val set_extra_command : t -> string option -> unit Lwt.t
val set_slack_webhooks : t -> Uri.t list -> unit Lwt.t
