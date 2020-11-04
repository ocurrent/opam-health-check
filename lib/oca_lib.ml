open Lwt.Infix

let (//) = Fpath.(/)

let rec list_map_cube f = function
  | x::(_::_ as xs) -> List.map (f x) xs @ list_map_cube f xs
  | [_] | [] -> []

let is_valid_filename file =
  not begin
    String.is_empty file ||
    Fpath.is_rel_seg file ||
    not (Fpath.is_seg file)
  end

let char_is_docker_compatible = function
  | 'a'..'z' | 'A'..'Z' | '0'..'9' -> true
  (* TODO: Add more *)
  | _ -> false

let get_files dirname =
  Lwt_unix.opendir (Fpath.to_string dirname) >>= fun dir ->
  let rec aux files =
    Lwt.catch begin fun () ->
      Lwt_unix.readdir dir >>= fun file ->
      if Fpath.is_rel_seg file then
        aux files
      else
        aux (file :: files)
    end begin function
    | End_of_file -> Lwt.return files
    | exn -> Lwt.fail exn
    end
  in
  aux [] >>= fun files ->
  Lwt_unix.closedir dir >|= fun () ->
  files

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

let rec rm_rf dirname =
  Lwt_unix.opendir (Fpath.to_string dirname) >>= fun dir ->
  Lwt.finalize begin fun () ->
    let rec rm_files () =
      Lwt_unix.readdir dir >>= function
      | "." | ".." -> rm_files ()
      | file ->
          let file = dirname // file in
          Lwt_unix.stat (Fpath.to_string file) >>= fun stat ->
          begin match stat.Unix.st_kind with
          | Unix.S_DIR -> rm_rf file
          | _ -> Lwt_unix.unlink (Fpath.to_string file)
          end >>= fun () ->
          rm_files ()
    in
    Lwt.catch rm_files begin function
    | End_of_file -> Lwt.return_unit
    | e -> Lwt.fail e
    end
  end begin fun () ->
    Lwt_unix.closedir dir >>= fun () ->
    Lwt_unix.rmdir (Fpath.to_string dirname)
  end

type timer = float ref

let timer_start () =
  ref (Unix.time ())

let timer_log timer c msg =
  let start_time = !timer in
  let end_time = Unix.time () in
  let time_span = end_time -. start_time in
  Lwt_io.write_line c ("Done. "^msg^" took: "^string_of_float time_span^" seconds") >|= fun () ->
  timer := Unix.time ()

let protocol_version = "2"
let default_server_name = "default" (* TODO: Just make it random instead?! *)
let default_html_port = "8080"
let default_admin_port = "9999"
let default_admin_name = "admin"
let default_auto_run_interval = 48 (* 48 hours *)
let default_processes = 72
let default_list_command = "opam list --available --installable --short --all-versions"
let localhost = "localhost"
