open Lwt.Infix

let pool = Lwt_pool.create 32 (fun () -> Lwt.return_unit)

let docker_build ~cached ~stderr ~img_name dockerfile =
  let cache = if cached then [] else ["--pull";"--no-cache"] in
  let stdin, fd = Lwt_unix.pipe () in
  let stdin = `FD_move stdin in
  Lwt_unix.set_close_on_exec fd;
  let proc = Oca_lib.exec ~stdin ~stdout:stderr ~stderr (["docker";"build"]@cache@["-t";img_name;"-"]) in
  Oca_lib.write_line_unix fd (Format.sprintf "%a" Dockerfile.pp dockerfile) >>= fun () ->
  Lwt_unix.close fd >>= fun () ->
  proc

let docker_run ~stdout ~stderr img cmd =
  Oca_lib.exec ~stdin:`Close ~stdout ~stderr ("docker"::"run"::"--rm"::img::cmd)

let rec read_lines fd =
  Lwt_io.read_line_opt fd >>= function
  | Some line -> read_lines fd >|= List.cons line
  | None -> Lwt.return_nil

let docker_run_to_str ~stderr ~img_name cmd =
  let fd, stdout = Lwt_unix.pipe () in
  let proc = docker_run ~stderr ~stdout img_name cmd in
  Lwt_unix.close stdout >>= fun () ->
  read_lines (Lwt_io.of_fd ~mode:Lwt_io.Input fd) >>= fun pkgs ->
  Lwt_unix.close fd >>= fun () ->
  proc >|= fun () ->
  pkgs

let get_pkgs ~stderr ~img_name =
  Oca_lib.write_line_unix stderr "Getting packages list..." >>= fun () ->
  docker_run_to_str ~stderr ~img_name [] >>= fun pkgs ->
  let rgen = Random.int_range (-1) 1 in
  let pkgs = List.filter Oca_lib.is_valid_filename pkgs in
  let pkgs = List.sort (fun _ _ -> Random.run rgen) pkgs in
  let nelts = string_of_int (List.length pkgs) in
  Oca_lib.write_line_unix stderr ("Package list retrieved. "^nelts^" elements to process.") >|= fun () ->
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

let run_job ~stderr ~img_name ~switch workdir pkg =
  Lwt_pool.use pool begin fun () ->
    Oca_lib.write_line_unix stderr ("Checking "^pkg^"...") >>= fun () ->
    let logfile = Server_workdirs.tmplogfile ~pkg ~switch workdir in
    Lwt_unix.openfile (Fpath.to_string logfile) Unix.[O_WRONLY; O_CREAT; O_TRUNC] 0o640 >>= fun stdout ->
    Lwt.catch begin fun () ->
      Lwt.finalize begin fun () ->
        docker_run ~stdout ~stderr:stdout img_name ["opam";"depext";"-ivy";pkg]
      end begin fun () ->
        Lwt_unix.close stdout
      end >>= fun () ->
      Lwt_unix.rename (Fpath.to_string logfile) (Fpath.to_string (Server_workdirs.tmpgoodlog ~pkg ~switch workdir))
    end begin function
    | Oca_lib.Process_failure ->
        is_partial_failure logfile >>= begin function
        | true -> Lwt_unix.rename (Fpath.to_string logfile) (Fpath.to_string (Server_workdirs.tmppartiallog ~pkg ~switch workdir))
        | false -> Lwt_unix.rename (Fpath.to_string logfile) (Fpath.to_string (Server_workdirs.tmpbadlog ~pkg ~switch workdir))
        end
    | e -> Lwt.fail e
    end
  end

let () =
  Lwt.async_exception_hook := begin fun e ->
    let msg = Printexc.to_string e in
    prerr_endline msg;
  end

let get_dockerfile ~conf switch =
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
  cmd "%s" (Server_configfile.list_command conf)

let with_stderr workdir f =
  Oca_lib.mkdir_p (Server_workdirs.ilogdir workdir) >>= fun () ->
  let logfile = Server_workdirs.ilogfile workdir in
  Lwt_unix.openfile (Fpath.to_string logfile) Unix.[O_WRONLY; O_CREAT; O_APPEND] 0o640 >>= fun stderr ->
  Lwt.finalize (fun () -> f ~stderr) (fun () -> Lwt_unix.close stderr)

let run ~on_finished ~conf workdir =
  let switches = Option.get_exn (Server_configfile.ocaml_switches conf) in
  let new_opam_repo_commit_hash = ref None in
  (* TODO: Add a lock here in case this function is triggered while it's already running *)
  Lwt.async begin fun () ->
    with_stderr workdir begin fun ~stderr ->
      switches |>
      Lwt_list.fold_left_s begin fun (jobs, i) switch ->
        let img_name = "opam-check-all-"^Intf.Compiler.to_string switch in
        let cached = match i with 0 -> false | _ -> true in
        docker_build ~stderr ~cached ~img_name (get_dockerfile ~conf switch) >>= fun () ->
        begin
          if not cached then
            docker_run_to_str ~stderr ~img_name ["git";"rev-parse";"HEAD"] >|= fun hash ->
            new_opam_repo_commit_hash := Some hash
          else
            Lwt.return_unit
        end >>= fun () ->
        Server_workdirs.init_base_job ~stderr ~switch workdir >>= fun () ->
        get_pkgs ~stderr ~img_name >|= fun pkgs ->
        (List.map (run_job ~stderr ~img_name ~switch workdir) pkgs @ jobs, succ i)
      end ([], 0) >>= fun (jobs, _) ->
      Lwt.join jobs >>= fun () ->
      let logdir = Server_workdirs.logdir workdir in
      let tmplogdir = Server_workdirs.tmplogdir workdir in
      (* TODO: replace by Oca_lib.rm_rf *)
      Oca_lib.exec ~stdin:`Close ~stdout:stderr ~stderr ["rm";"-rf";Fpath.to_string logdir] >>= fun () ->
      Lwt_unix.rename (Fpath.to_string tmplogdir) (Fpath.to_string logdir) >>= fun () ->
      begin match !new_opam_repo_commit_hash with
      | Some [hash] -> Server_configfile.set_opam_repo_commit_hash conf hash
      | Some _ -> Server_configfile.set_opam_repo_commit_hash conf "[internal failure]"
      | None -> Lwt.return_unit
      end >|= fun () ->
      on_finished workdir
    end
  end;
  Lwt.return_unit
