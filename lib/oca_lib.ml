open Lwt.Infix

let is_valid_filename file =
  not begin
    String.is_empty file ||
    Fpath.is_rel_seg file ||
    not (Fpath.is_seg file)
  end

let mkdir_p dir =
  let rec aux base = function
    | [] ->
        Lwt.return_unit
    | x::xs ->
        let dir = Fpath.add_seg base x in
        Lwt.catch begin fun () ->
          Lwt_unix.mkdir (Fpath.to_string dir) 0o750
        end begin function
        | Unix.Unix_error (Unix.EEXIST, _, _) -> Lwt.return_unit
        | e -> Lwt.fail e
        end >>= fun () ->
        aux dir xs
  in
  match Fpath.segs dir with
  | ""::dirs -> aux Fpath.(v dir_sep) dirs
  | dirs -> aux (Fpath.v Filename.current_dir_name) dirs

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

exception Process_failure of int
exception Internal_failure

let exec ~stdin ~stdout ~stderr cmd =
  let stdin = proc_fd_of_unix stdin in
  let stdout = proc_fd_of_unix (`FD_copy stdout) in
  let stderr_lwt = stderr in
  let stderr = proc_fd_of_unix (`FD_copy stderr) in
  let proc =
    Lwt_process.exec ~stdin ~stdout ~stderr ("", Array.of_list cmd) >>= function
    | Unix.WEXITED 0 ->
        Lwt.return_unit
    | Unix.WEXITED e ->
        let cmd = String.concat " " cmd in
        write_line_unix stderr_lwt ("Command '"^cmd^"' failed.") >>= fun () ->
        Lwt.fail (Process_failure e)
    | Unix.WSIGNALED _ | Unix.WSTOPPED _ ->
        Lwt.fail Internal_failure
  in
  (* NOTE: any processes shouldn't take more than 5 hours *)
  let timeout =
    let hours = 5 in
    Lwt_unix.sleep (float_of_int (hours * 60 * 60)) >>= fun () ->
    let cmd = String.concat " " cmd in
    write_line_unix stderr_lwt ("Command '"^cmd^"' timed-out ("^string_of_int hours^" hours).") >>= fun () ->
    Lwt.fail Internal_failure
  in
  Lwt.pick [timeout; proc]

let protocol_version = "2"
let default_html_port = "8080"
let default_admin_port = "9999"
let default_admin_name = "admin"
let default_auto_run_interval = 48 (* 48 hours *)
let default_list_command = "opam list --available --short --all-versions"
let localhost = "localhost"
