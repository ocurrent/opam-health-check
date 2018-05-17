open Containers
open Lwt.Infix

let write_line_unix fd s =
  let fd = Lwt_io.of_fd ~mode:Lwt_io.Output fd in
  Lwt_io.write_line fd s >>= fun () ->
  Lwt_io.flush fd

let pool = Lwt_pool.create 32 (fun () -> Lwt.return_unit)

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

let docker_build ~stderr ~img_name dockerfile =
  let stdin, fd = Lwt_unix.pipe () in
  let stdin = `FD_move stdin in
  write_line_unix fd dockerfile >>= fun () ->
  Lwt_unix.close fd >>= fun () ->
  exec ~stdin ~stdout:stderr ~stderr ["docker";"build";"-t";img_name;"-"]

let docker_run ~stdout ~stderr img cmd =
  exec ~stdin:`Close ~stdout ~stderr ("docker"::"run"::"--rm"::img::cmd)

let get_pkgs ~stderr ~dockerfile =
  let md5 = Digest.to_hex (Digest.string dockerfile) in
  let img_name = "opam-check-all-" ^ md5 in
  docker_build ~stderr ~img_name dockerfile >>= fun () ->
  let fd, stdout = Lwt_unix.pipe () in
  write_line_unix stderr "Getting packages list..." >>= fun () ->
  docker_run ~stdout ~stderr img_name [] >>= fun () ->
  Lwt_unix.close stdout >>= fun () ->
  Lwt_io.read (Lwt_io.of_fd ~mode:Lwt_io.Input fd) >>= fun pkgs ->
  Lwt_unix.close fd >|= fun () ->
  (img_name, String.split_on_char '\n' pkgs)

let job_queue = Queue.create ()

let rec get_jobs ~stderr ~img_name ~logdir ~gooddir ~baddir jobs = function
  | [] ->
      Lwt_pool.use pool begin fun () ->
        Lwt.join jobs >>= fun () ->
        Cache.clear ();
        begin match Queue.pop job_queue with
        | f -> f ()
        | exception Queue.Empty -> ()
        end;
        Lwt_unix.close stderr
      end
  | pkg::pkgs ->
      let job =
        Lwt_pool.use pool begin fun () ->
          let goodlog = Filename.concat gooddir pkg in
          let badlog = Filename.concat baddir pkg in
          Lwt_unix.file_exists goodlog >>= fun goodlog_exists ->
          Lwt_unix.file_exists badlog >>= fun badlog_exists ->
          if goodlog_exists || badlog_exists then begin
            write_line_unix stderr (pkg^" has already been checked. Skipping...")
          end else begin
            write_line_unix stderr ("Checking "^pkg^"...") >>= fun () ->
            let logfile = Filename.concat logdir pkg in
            Lwt_unix.openfile logfile [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o640 >>= fun stdout ->
            Lwt.finalize begin fun () ->
              Lwt.catch begin fun () ->
                docker_run ~stdout ~stderr:stdout img_name ["opam";"depext";"-ivy";pkg] >>= fun () ->
                Lwt_unix.rename logfile goodlog
              end begin function
              | Process_failure -> Lwt_unix.rename logfile badlog
              | e -> Lwt.fail e
              end
            end begin fun () ->
              Lwt_unix.close stdout
            end
          end
        end
      in
      (* TODO: Delete the directory first *)
      get_jobs ~stderr ~img_name ~logdir ~gooddir ~baddir (job :: jobs) pkgs

let async_proc ~stderr f =
  let aux () =
    let old = !Lwt.async_exception_hook in
    let stderr = Lwt_unix.unix_file_descr stderr in
    Lwt.async_exception_hook := begin fun e ->
      let msg = Printexc.to_string e in
      let chan = Unix.out_channel_of_descr stderr in
      output_string chan (msg^"\n");
      flush chan;
      Unix.close stderr
    end;
    Lwt.async f;
    Lwt.async_exception_hook := old
  in
  match Queue.is_empty job_queue with
  | true -> aux ()
  | false -> Queue.add aux job_queue

let is_valid_name_char = function
  | '0'..'9'
  | 'a'..'z'
  | 'A'..'Z'
  | '.' | '+' | '-' | '~' -> true
  | _ -> false

let is_valid_name name =
  not (String.is_empty name) &&
  String.for_all is_valid_name_char name &&
  not (String.equal name Filename.parent_dir_name) &&
  not (String.equal name Filename.current_dir_name)

let check ~logdir ~dockerfile name =
  if is_valid_name name then
    failwith "Name is not valid";
  let logfile = Filename.concat logdir (name^".log") in
  let logdir = Filename.concat logdir name in
  Lwt_unix.openfile logfile [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o640 >>= fun stderr ->
  Lwt.catch begin fun () ->
    let gooddir = Filename.concat logdir "good" in
    let baddir = Filename.concat logdir "bad" in
    Oca_lib.mkdir_p gooddir >>= fun () ->
    Oca_lib.mkdir_p baddir >|= fun () ->
    async_proc ~stderr begin fun () ->
      get_pkgs ~stderr ~dockerfile >>= fun (img_name, pkgs) ->
      get_jobs ~stderr ~img_name ~logdir ~gooddir ~baddir [] pkgs
    end
  end begin fun e ->
    write_line_unix stderr (Printexc.to_string e) >>= fun () ->
    Lwt_unix.close stderr >>= fun () ->
    Lwt.fail e
  end
