val mkdir_p : string -> unit Lwt.t

val protocol_version : string
val default_html_port : string
val default_admin_port : string
val default_admin_name : string
val localhost : string

val keysdir : workdir:string -> string
val keyfile : keysdir:string -> username:string -> string
