val list_map_cube : ('a -> 'a -> 'b) -> 'a list -> 'b list

val is_valid_filename : string -> bool
val char_is_docker_compatible : char -> bool

val get_files : Fpath.t -> string list Lwt.t
val mkdir_p : Fpath.t -> unit Lwt.t
val rm_rf : Fpath.t -> unit Lwt.t

type timer

val timer_start : unit -> timer
val timer_log : timer -> Lwt_io.output_channel -> string -> unit Lwt.t

val protocol_version : string
val default_server_name : string
val default_html_port : string
val default_public_url : string
val default_admin_port : string
val default_admin_name : string
val default_auto_run_interval : int
val default_processes : int
val default_list_command : string
val localhost : string
