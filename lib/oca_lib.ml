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

let rec scan_dir dirname =
  get_files dirname >>= fun files ->
  Lwt_list.fold_left_s (fun acc file ->
    let file = Fpath.add_seg dirname file in
    Lwt_unix.stat (Fpath.to_string file) >>= function
    | {Unix.st_kind = Unix.S_DIR; _} -> scan_dir file >|= fun files -> files @ acc
    | {Unix.st_kind = Unix.S_REG; _} -> Lwt.return (Fpath.to_string file :: acc)
    | _ -> assert false
  ) [] files

let pread ~timeout cmd f =
  Lwt_process.with_process_in ~timeout ~stdin:`Close ("", Array.of_list cmd) begin fun proc ->
    f proc#stdout >>= fun res ->
    proc#close >>= function
    | Unix.WEXITED 0 ->
        Lwt.return res
    | Unix.WEXITED n ->
        let cmd = String.concat " " cmd in
        prerr_endline ("Command '"^cmd^"' failed (exit status: "^string_of_int n^".");
        Lwt.fail (Failure "process failure")
    | Unix.WSIGNALED n | Unix.WSTOPPED n ->
        let cmd = String.concat " " cmd in
        prerr_endline ("Command '"^cmd^"' killed by a signal (n°"^string_of_int n^")");
        Lwt.fail (Failure "process failure")
  end

let scan_tpxz_archive archive =
  pread ~timeout:60. ["pixz"; "-l"; Fpath.to_string archive] begin fun c ->
    let rec aux acc =
      Lwt_io.read_line_opt c >>= function
      | None -> Lwt.return acc (* Note: We don't care about the line ordering *)
      | Some line -> aux (line :: acc)
    in
    aux []
  end

let random_access_tpxz_archive ~file archive =
  let file = Filename.quote file in
  let archive = Filename.quote (Fpath.to_string archive) in
  pread ~timeout:60. ["sh"; "-c"; "pixz -x "^file^" -i "^archive^" | tar -xO"] (Lwt_io.read ?count:None)

let compress_tpxz_archive ~directories archive =
  pread ~timeout:3600. ("tar" :: "-Ipixz" :: "-cf" :: Fpath.to_string archive :: List.map Fpath.to_string directories) begin fun _ ->
    (* TODO: Do not use pread *)
    Lwt.return ()
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
let default_public_url = "http://check.ocamllabs.io"
let default_admin_port = "9999"
let default_admin_name = "admin"
let default_auto_run_interval = 48 (* 48 hours *)
let default_processes = 200
let default_list_command = "opam list --available --installable --short --all-versions"
let localhost = "localhost"
