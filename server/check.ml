open Containers
open Lwt.Infix

let pool = Lwt_pool.create 32 (fun () -> Lwt.return_unit)

let docker_build ~stderr ~img_name dockerfile =
  let stdin, fd = Lwt_unix.pipe () in
  let stdin = `FD_move stdin in
  Oca_lib.write_line_unix fd dockerfile >>= fun () ->
  Lwt_unix.close fd >>= fun () ->
  Oca_lib.exec ~stdin ~stdout:stderr ~stderr ["docker";"build";"-t";img_name;"-"]

let docker_run ~stdout ~stderr img cmd =
  Oca_lib.exec ~stdin:`Close ~stdout ~stderr ("docker"::"run"::"--rm"::img::cmd)

let get_pkgs ~stderr ~dockerfile =
  let md5 = Digest.to_hex (Digest.string dockerfile) in
  let img_name = "opam-check-all-" ^ md5 in
  docker_build ~stderr ~img_name dockerfile >>= fun () ->
  let fd, stdout = Lwt_unix.pipe () in
  Oca_lib.write_line_unix stderr "Getting packages list..." >>= fun () ->
  docker_run ~stdout ~stderr img_name [] >>= fun () ->
  Lwt_unix.close stdout >>= fun () ->
  Lwt_io.read (Lwt_io.of_fd ~mode:Lwt_io.Input fd) >>= fun pkgs ->
  Lwt_unix.close fd >|= fun () ->
  (img_name, String.split_on_char '\n' pkgs)

let job_tbl = Hashtbl.create 32

let rec get_jobs ~stderr ~img_name ~switch workdir jobs = function
  | [] ->
      Lwt_pool.use pool begin fun () ->
        Lwt.join jobs >>= fun () ->
        let logdir = Server_workdirs.switchlogdir ~switch workdir in
        let tmplogdir = Server_workdirs.tmpswitchlogdir ~switch workdir in
        (* TODO: replace by Oca_lib.rm_rf *)
        Oca_lib.exec ~stdin:`Close ~stdout:stderr ~stderr ["rm";"-rf";logdir] >>= fun () ->
        Lwt_unix.rename tmplogdir logdir >>= fun () ->
        Cache.clear ();
        Hashtbl.remove job_tbl switch;
        Lwt_unix.close stderr
      end
  | pkg::pkgs ->
      let job =
        Lwt_pool.use pool begin fun () ->
          Oca_lib.write_line_unix stderr ("Checking "^pkg^"...") >>= fun () ->
          let logfile = Server_workdirs.logfile ~pkg ~switch workdir in
          Lwt_unix.openfile logfile Unix.[O_WRONLY; O_CREAT; O_TRUNC] 0o640 >>= fun stdout ->
          Lwt.finalize begin fun () ->
            Lwt.catch begin fun () ->
              docker_run ~stdout ~stderr:stdout img_name ["opam";"depext";"-ivy";pkg] >>= fun () ->
              Lwt_unix.rename logfile (Server_workdirs.tmpgoodlog ~pkg ~switch workdir)
            end begin function
            | Oca_lib.Process_failure -> Lwt_unix.rename logfile (Server_workdirs.tmpbadlog ~pkg ~switch workdir)
            | e -> Lwt.fail e
            end
          end begin fun () ->
            Lwt_unix.close stdout
          end
        end
      in
      get_jobs ~stderr ~img_name ~switch workdir (job :: jobs) pkgs

let () =
  Lwt.async_exception_hook := begin fun e ->
    let msg = Printexc.to_string e in
    prerr_endline msg;
    (* TODO: Close stderr *)
  end

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

let check workdir ~dockerfile name =
  if not (is_valid_name name) then
    failwith "Name is not valid";
  if Hashtbl.mem job_tbl name then
    failwith "A job with the same name is already running";
  Oca_lib.mkdir_p (Server_workdirs.switchilogdir ~switch:name workdir) >>= fun () ->
  let logfile = Server_workdirs.ilogfile ~switch:name workdir in
  Lwt_unix.openfile logfile Unix.[O_WRONLY; O_CREAT; O_TRUNC; O_EXCL] 0o640 >>= fun stderr ->
  Server_workdirs.init_base_job ~switch:name ~stderr workdir >|= fun () ->
  Lwt.async begin fun () ->
    Hashtbl.add job_tbl name ();
    get_pkgs ~stderr ~dockerfile >>= fun (img_name, pkgs) ->
    get_jobs ~stderr ~img_name ~switch:name workdir [] pkgs
  end
