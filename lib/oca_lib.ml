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

let write_line_unix fd s =
  let fd = Lwt_io.of_fd ~mode:Lwt_io.Output fd in
  Lwt_io.write_line fd s >>= fun () ->
  Lwt_io.flush fd

let proc_fd_of_unix = function
  | `Close -> `Close
  | `Dev_null -> `Dev_null
  | `FD_move fd -> `FD_move (Lwt_unix.unix_file_descr fd)
  | `FD_copy fd -> `FD_copy (Lwt_unix.unix_file_descr fd)
  | `Keep -> `Keep

exception Process_failure

let exec ~stdin ~stdout ~stderr cmd =
  let stdin = proc_fd_of_unix stdin in
  let stdout = proc_fd_of_unix (`FD_copy stdout) in
  let stderr_lwt = stderr in
  let stderr = proc_fd_of_unix (`FD_copy stderr) in
  Lwt_process.exec ~stdin ~stdout ~stderr ("", Array.of_list cmd) >>= function
  | Unix.WEXITED 0 ->
      Lwt.return_unit
  | _ ->
      let cmd = String.concat " " cmd in
      write_line_unix stderr_lwt ("Command '"^cmd^"' failed.") >>= fun () ->
      Lwt.fail Process_failure

let protocol_version = "1"
let default_html_port = "8080"
let default_admin_port = "9999"
let default_admin_name = "admin"
let localhost = "localhost"
