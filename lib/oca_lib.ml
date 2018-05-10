open Containers
open Lwt.Infix

let mkdir_p dir =
  let rec aux base = function
    | [] ->
        Lwt.return_unit
    | x::xs ->
        let dir = Filename.concat base x in
        Lwt.catch begin fun () ->
          Lwt_unix.mkdir dir 0o750
        end begin function
        | Unix.Unix_error (Unix.EEXIST, _, _) -> Lwt.return_unit
        | e -> Lwt.fail e
        end >>= fun () ->
        aux dir xs
  in
  match String.Split.list_cpy ~by:Filename.dir_sep dir with
  | ""::dirs -> aux Filename.dir_sep dirs
  | dirs -> aux "" dirs

let protocol_version = "1"
let default_html_port = "8080"
let default_admin_port = "9999"
let default_admin_name = "admin"
let localhost = "localhost"

let keysdir ~workdir = Filename.concat workdir "keys"
let keyfile ~keysdir ~username = Filename.concat keysdir (username^".key")
