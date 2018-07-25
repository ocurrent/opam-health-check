open Lwt.Infix

let pool = Lwt_pool.create 32 (fun () -> Lwt.return_unit)

let docker_build ~no_cache ~stderr ~img_name dockerfile =
  let no_cache = if no_cache then ["--pull";"--no-cache"] else [] in
  let stdin, fd = Lwt_unix.pipe () in
  let stdin = `FD_move stdin in
  Lwt_unix.set_close_on_exec fd;
  let proc = Oca_lib.exec ~stdin ~stdout:stderr ~stderr (["docker";"build"]@no_cache@["-t";img_name;"-"]) in
  Oca_lib.write_line_unix fd dockerfile >>= fun () ->
  Lwt_unix.close fd >>= fun () ->
  proc

let docker_run ~stdout ~stderr img cmd =
  Oca_lib.exec ~stdin:`Close ~stdout ~stderr ("docker"::"run"::"--rm"::img::cmd)

let rec read_lines fd =
  Lwt_io.read_line_opt fd >>= function
  | Some line -> read_lines fd >|= List.cons line
  | None -> Lwt.return_nil

(* NOTE: We need that so that docker won't build two exact things twice *)
let build_task = ref Lwt.return_unit

let get_pkgs ~no_cache ~stderr ~dockerfile =
  let md5 = Digest.to_hex (Digest.string dockerfile) in
  let img_name = "opam-check-all-" ^ md5 in
  Lwt.catch (fun () -> !build_task) (fun _ -> Lwt.return_unit) >>= fun () ->
  let build = docker_build ~no_cache ~stderr ~img_name dockerfile in
  build_task := build;
  build >>= fun () ->
  Oca_lib.write_line_unix stderr "Getting packages list..." >>= fun () ->
  let fd, stdout = Lwt_unix.pipe () in
  let proc = docker_run ~stderr ~stdout img_name [] in
  Lwt_unix.close stdout >>= fun () ->
  read_lines (Lwt_io.of_fd ~mode:Lwt_io.Input fd) >>= fun pkgs ->
  Lwt_unix.close fd >>= fun () ->
  let pkgs = List.filter Oca_lib.is_valid_filename pkgs in
  let nelts = string_of_int (List.length pkgs) in
  Oca_lib.write_line_unix stderr ("Package list retrieved. "^nelts^" elements to process.") >>= fun () ->
  proc >|= fun () ->
  (img_name, pkgs)

let is_partial_failure logfile =
  Lwt_io.with_file ~mode:Lwt_io.Input (Fpath.to_string logfile) begin fun ic ->
    let rec lookup () =
      Lwt_io.read_line_opt ic >>= function
      | Some "+- The following actions were aborted" -> Lwt.return_true
      | Some _ -> lookup ()
      | None -> Lwt.return_false
    in
    lookup ()
  end

module Jobs = Hashtbl.Make (struct
    type t = Intf.Compiler.t

    let hash = Hashtbl.hash
    let equal = Intf.Compiler.equal
  end)

let job_tbl = Jobs.create 32

let rec get_jobs ~on_finished ~stderr ~img_name ~switch workdir jobs = function
  | [] ->
      Lwt_pool.use pool begin fun () ->
        Lwt.join jobs >>= fun () ->
        let logdir = Server_workdirs.switchlogdir ~switch workdir in
        let tmplogdir = Server_workdirs.tmpswitchlogdir ~switch workdir in
        (* TODO: replace by Oca_lib.rm_rf *)
        Oca_lib.exec ~stdin:`Close ~stdout:stderr ~stderr ["rm";"-rf";Fpath.to_string logdir] >>= fun () ->
        Lwt_unix.rename (Fpath.to_string tmplogdir) (Fpath.to_string logdir) >>= fun () ->
        on_finished workdir;
        Jobs.remove job_tbl switch;
        Lwt_unix.close stderr
      end
  | pkg::pkgs ->
      let job =
        Lwt_pool.use pool begin fun () ->
          Oca_lib.write_line_unix stderr ("Checking "^pkg^"...") >>= fun () ->
          let logfile = Server_workdirs.tmplogfile ~pkg ~switch workdir in
          Lwt_unix.openfile (Fpath.to_string logfile) Unix.[O_WRONLY; O_CREAT; O_TRUNC] 0o640 >>= fun stdout ->
          Lwt.finalize begin fun () ->
            Lwt.catch begin fun () ->
              docker_run ~stdout ~stderr:stdout img_name ["opam";"depext";"-ivy";pkg] >>= fun () ->
              Lwt_unix.rename (Fpath.to_string logfile) (Fpath.to_string (Server_workdirs.tmpgoodlog ~pkg ~switch workdir))
            end begin function
            | Oca_lib.Process_failure ->
                is_partial_failure logfile >>= begin function
                | true -> Lwt_unix.rename (Fpath.to_string logfile) (Fpath.to_string (Server_workdirs.tmppartiallog ~pkg ~switch workdir))
                | false -> Lwt_unix.rename (Fpath.to_string logfile) (Fpath.to_string (Server_workdirs.tmpbadlog ~pkg ~switch workdir))
                end
            | e -> Lwt.fail e
            end
          end begin fun () ->
            Lwt_unix.close stdout
          end
        end
      in
      get_jobs ~on_finished ~stderr ~img_name ~switch workdir (job :: jobs) pkgs

let () =
  Lwt.async_exception_hook := begin fun e ->
    let msg = Printexc.to_string e in
    prerr_endline msg;
    (* TODO: Close stderr *)
  end

let check workdir ~no_cache ~on_finished ~dockerfile name =
  if Jobs.mem job_tbl name then
    failwith "A job with the same name is already running";
  Oca_lib.mkdir_p (Server_workdirs.switchilogdir ~switch:name workdir) >>= fun () ->
  let logfile = Server_workdirs.ilogfile ~switch:name workdir in
  Lwt_unix.openfile (Fpath.to_string logfile) Unix.[O_WRONLY; O_CREAT; O_TRUNC; O_EXCL] 0o640 >>= fun stderr ->
  Server_workdirs.init_base_job ~switch:name ~stderr workdir >|= fun () ->
  Lwt.async begin fun () ->
    Jobs.add job_tbl name ();
    get_pkgs ~no_cache ~stderr ~dockerfile >>= fun (img_name, pkgs) ->
    let dfile = Server_workdirs.dockerfile ~switch:name workdir in
    Lwt_io.with_file ~flags:Unix.[O_CREAT] ~mode:Lwt_io.Output (Fpath.to_string dfile) (fun c -> Lwt_io.write c dockerfile) >>= fun () ->
    get_jobs ~on_finished ~stderr ~img_name ~switch:name workdir [] pkgs
  end
