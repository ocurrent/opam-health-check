type t

val from_workdir : Server_workdirs.t -> t

val port : t -> int
val admin_port : t -> int
val list_command : t -> string
val ocaml_switches : t -> Intf.Compiler.t list option
val opam_repo_commit_hash : t -> string option

val set_ocaml_switches : t -> Intf.Compiler.t list -> unit Lwt.t
val set_default_ocaml_switches : t -> (unit -> Intf.Compiler.t list Lwt.t) -> unit Lwt.t
val set_list_command : t -> string -> unit Lwt.t
val set_opam_repo_commit_hash : t -> string -> unit Lwt.t
