open Lwt.Infix

let pool = Lwt_pool.create 16 (fun () -> Lwt.return_unit)
let queue = ref Lwt.return_unit

let docker_build ~stderr ~img_name dockerfile =
  let stdin, fd = Lwt_unix.pipe () in
  let stdin = `FD_move stdin in
  Lwt_unix.set_close_on_exec fd;
  let proc = Oca_lib.exec ~stdin ~stdout:stderr ~stderr (["docker";"build";"--pull";"-t";img_name;"-"]) in
  Oca_lib.write_line_unix fd (Format.sprintf "%a" Dockerfile.pp dockerfile) >>= fun () ->
  Lwt_unix.close fd >>= fun () ->
  proc

let docker_run ~stdout ~stderr img cmd =
  Oca_lib.exec ~stdin:`Close ~stdout ~stderr ("docker"::"run"::"--rm"::img::cmd)

let rec read_lines fd =
  Lwt_io.read_line_opt fd >>= function
  | Some line -> read_lines fd >|= List.cons line
  | None -> Lwt.return_nil

let get_pkgs ~stderr ~img_name =
  Oca_lib.write_line_unix stderr "Getting packages list..." >>= fun () ->
  let fd, stdout = Lwt_unix.pipe () in
  let proc = docker_run ~stderr ~stdout img_name [] in
  Lwt_unix.close stdout >>= fun () ->
  read_lines (Lwt_io.of_fd ~mode:Lwt_io.Input fd) >>= fun pkgs ->
  Lwt_unix.close fd >>= fun () ->
  let rgen = Random.int_range (-1) 1 in
  let pkgs = List.filter Oca_lib.is_valid_filename pkgs in
  let pkgs = List.sort (fun _ _ -> Random.run rgen) pkgs in
  let nelts = string_of_int (List.length pkgs) in
  Oca_lib.write_line_unix stderr ("Package list retrieved. "^nelts^" elements to process.") >>= fun () ->
  proc >|= fun () ->
  pkgs

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
  end

let get_dockerfile switch =
  let open Dockerfile in
  from "ocaml/opam2:debian-unstable-opam" @@
  run "sudo apt-get update" @@
  workdir "opam-repository" @@
  run "git pull origin master" @@
  run "opam admin cache" @@
  run "echo 'wrap-build-commands: []' >> ~/.opamrc" @@
  run "echo 'wrap-install-commands: []' >> ~/.opamrc" @@
  run "echo 'wrap-remove-commands: []' >> ~/.opamrc" @@
  run "opam init -yac %s ." (Intf.Compiler.to_string switch) @@
  run "echo 'archive-mirrors: [\"file:///home/opam/opam-repository/cache\"]' >> /home/opam/.opam/config" @@
  run "opam install -y opam-depext" @@
  cmd "opam list --installable --available --short --all-versions"

let acquire_queue f =
  (* TODO: Use Lwt.catch to ensure the queue can continue if anything happens *)
  Lwt.async (fun () -> !queue >|= fun () -> queue := Lwt_pool.use pool f)

let with_stderr ~switch workdir f =
  Oca_lib.mkdir_p (Server_workdirs.switchilogdir ~switch workdir) >>= fun () ->
  let logfile = Server_workdirs.ilogfile ~switch workdir in
  Lwt_unix.openfile (Fpath.to_string logfile) Unix.[O_WRONLY; O_CREAT; O_APPEND] 0o640 >>= fun stderr ->
  Lwt.finalize (fun () -> f ~stderr) (fun () -> Lwt_unix.close stderr)

let opam_repo_commit_hash = ref None
let ocaml_switches = ref []
let set_ocaml_switches workdir switches =
  let switches = List.sort Intf.Compiler.compare switches in
  Lwt_list.map_s begin fun switch ->
    let img_name = "opam-check-all-"^Intf.Compiler.to_string switch in
    acquire_queue (fun () -> with_stderr ~switch workdir (docker_build ~img_name (get_dockerfile switch)));
    Lwt.return (switch, img_name)
  end switches >|=
  (:=) ocaml_switches

let run ~on_finished workdir =
  Lwt_list.iter_s begin fun (switch, img_name) ->
    Lwt_unix.sleep 1. >|= fun () -> (* TODO: Get rid of this and name the log files better (without timestamps) *)
    Lwt.async begin fun () ->
      !queue >>= fun () ->
      with_stderr ~switch workdir begin fun ~stderr ->
        Server_workdirs.init_base_job ~stderr ~switch workdir >|= fun () ->
        get_pkgs ~stderr ~img_name >>=
        get_jobs ~stderr ~on_finished ~img_name ~switch workdir []
      end
    end
  end !ocaml_switches
