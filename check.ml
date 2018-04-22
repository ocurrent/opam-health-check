open Containers
open Lwt.Infix

type img = string
type pkgs = string list

let write_line_unix fd s =
  Lwt_io.write_line (Lwt_io.of_fd ~mode:Lwt_io.Output fd) s

let mkdir_p dir =
  let rec aux base = function
    | [] ->
        Lwt.return_unit
    | x::xs ->
        let dir = Filename.concat base x in
        Lwt_unix.mkdir dir 0o750 >>= fun () ->
        aux dir xs
  in
  match String.Split.list_cpy ~by:Filename.dir_sep dir with
  | ""::dirs -> aux Filename.dir_sep dirs
  | dirs -> aux "" dirs

let pool = Lwt_pool.create 32 (fun () -> Lwt.return_unit)

let exec_in ~stdin ~stdout ~stderr cmd =
  Lwt_process.exec ~stdin ~stdout ~stderr ("", Array.of_list cmd) >>= function
  | Unix.WEXITED 0 ->
      Lwt.return (Ok ())
  | _ ->
      let cmd = String.concat " " cmd in
      Lwt_io.write_line Lwt_io.stderr ("Command '"^cmd^"' failed.") >>= fun () ->
      Lwt.return (Error ())

let docker_build ~stdout args dockerfile =
  let stdout = Lwt_unix.unix_file_descr stdout in
  let stdin, fd = Lwt_unix.pipe_out () in
  let stdin = `FD_move stdin in
  let stderr = `FD_copy stdout in
  let stdout = `FD_copy stdout in
  let fd = Lwt_io.of_fd ~mode:Lwt_io.Output fd in
  Lwt_io.write_line fd dockerfile >>= fun () ->
  Lwt_io.close fd >>= fun () ->
  exec_in ~stdin ~stdout ~stderr ("docker"::"build"::args@["-"])

let docker_run ~stdout img cmd =
  let stdout = Lwt_unix.unix_file_descr stdout in
  let stderr = `FD_move stdout in
  let stdout = `FD_copy stdout in
  exec_in ~stdin:`Close ~stdout ~stderr ("docker"::"run"::"--rm"::img::cmd)

let get_pkgs ~stdout ~dockerfile =
  let md5 = Digest.to_hex (Digest.string dockerfile) in
  let img_name = "opam-check-all-" ^ md5 in
  docker_build ~stdout ["-t"; img_name] dockerfile >>= fun _ ->
  write_line_unix stdout "Getting packages list..." >>= fun () ->
  Lwt_process.pread ("", [|"docker"; "run"; img_name|]) >|= fun pkgs ->
  (img_name, String.split_on_char '\n' pkgs)

(* TODO: Redirect everything to a per user & jobs log *)
let rec get_jobs ~stdout ~img_name ~logdir ~gooddir ~baddir = function
  | [] ->
      Lwt_pool.use pool begin fun () ->
        Cache.clear ();
        Lwt_unix.close stdout
      end
  | pkg::pkgs ->
      Lwt_pool.use pool begin fun () ->
        let goodlog = Filename.concat gooddir pkg in
        let badlog = Filename.concat baddir pkg in
        Lwt_unix.file_exists goodlog >>= fun goodlog_exists ->
        Lwt_unix.file_exists badlog >>= fun badlog_exists ->
        if goodlog_exists || badlog_exists then begin
          write_line_unix stdout (pkg^" has already been checked. Skipping...")
        end else begin
          write_line_unix stdout ("Checking "^pkg^"...") >>= fun () ->
          let logfile = Filename.concat logdir pkg in
          Lwt_unix.openfile logfile [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o640 >>= fun stdout ->
          docker_run ~stdout img_name ["opam";"depext";"-ivy";pkg] >>= begin function
          | Ok () -> Lwt_unix.rename logfile goodlog
          | Error () -> Lwt_unix.rename logfile badlog
          end
        end
      end |> Lwt.ignore_result;
      get_jobs ~stdout ~img_name ~logdir ~gooddir ~baddir pkgs

let check ~logdir ~dockerfile name =
  let logfile = Filename.concat logdir (name^".log") in
  Lwt_unix.openfile logfile [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o640 >>= fun stdout ->
  get_pkgs ~stdout ~dockerfile >>= fun (img_name, pkgs) ->
  let gooddir = Filename.concat logdir "good" in
  let baddir = Filename.concat logdir "bad" in
  mkdir_p gooddir >>= fun () ->
  mkdir_p baddir >>= fun () ->
  get_jobs ~stdout ~img_name ~logdir ~gooddir ~baddir pkgs
