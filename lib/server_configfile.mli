type t

val from_workdir : Server_workdirs.t -> t

val name : t -> string
val port : t -> int
val admin_port : t -> int
val auto_run_interval : t -> int
val processes : t -> int
val list_command : t -> string
val extra_command : t -> string option
val ocaml_switches : t -> Intf.Compiler.t list option
val opam_repo_commit_hash : t -> string option
val opam_repo_old_commit_hash : t -> string option
val slack_webhooks : t -> Uri.t list

val set_auto_run_interval : t -> int -> unit Lwt.t
val set_processes : t -> int -> unit Lwt.t
val set_ocaml_switches : t -> Intf.Compiler.t list -> unit Lwt.t
val set_default_ocaml_switches : t -> (unit -> Intf.Compiler.t list Lwt.t) -> unit Lwt.t
val set_list_command : t -> string -> unit Lwt.t
val set_extra_command : t -> string option -> unit Lwt.t
val set_opam_repo_commit_hash : t -> string -> unit Lwt.t
val set_slack_webhooks : t -> Uri.t list -> unit Lwt.t
