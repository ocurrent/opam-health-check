open Lwt.Infix

let pool = Lwt_pool.create 32 (fun () -> Lwt.return_unit)

let docker_build ~cached ~stderr ~img_name dockerfile =
  let cache = if cached then [] else ["--pull";"--no-cache"] in
  let stdin, fd = Lwt_unix.pipe () in
  let stdin = `FD_move stdin in
  Lwt_unix.set_close_on_exec fd;
  begin
    if not cached then
      Oca_lib.exec ~stdin:`Close ~stdout:stderr ~stderr ["docker";"system";"prune";"-af";"--volumes"]
    else
      Lwt.return_unit
  end >>= fun () ->
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

let get_img_name switch =
  "opam-check-all-"^Intf.Compiler.to_string switch

let get_pkgs ~stderr switch =
  let img_name = get_img_name switch in
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

let run_job ~stderr ~switch workdir pkg =
  let img_name = get_img_name switch in
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
    prerr_endline ("Async exception raised: "^msg);
  end

let get_dockerfile ~conf switch =
  let open Dockerfile in
  from "ocaml/opam2:debian-unstable-opam AS base" @@
  run "sudo apt-get update" @@
  run "sudo apt-get install -y coinor-libsymphony-dev ocaml" @@
  workdir "/tmp" @@
  run "curl -L https://github.com/ocaml/opam/releases/download/2.0.2/opam-full-2.0.2.tar.gz -o opam-full-2.0.2.tar.gz" @@
  run "tar xvf opam-full-2.0.2.tar.gz" @@
  workdir "/tmp/opam-full-2.0.2" @@
  env ["MCCS_BACKENDS", "SYMPHONY"] @@
  run "./configure" @@
  run "make lib-ext" @@
  run "make" @@
  from "ocaml/opam2:debian-unstable-opam" @@
  copy ~from:"base" ~src:["/tmp/opam-full-2.0.2/opam"] ~dst:"/usr/bin/opam" () @@
  copy ~from:"base" ~src:["/tmp/opam-full-2.0.2/opam-installer"] ~dst:"/usr/bin/opam-installer" () @@
  run "sudo apt-get update" @@
  run "sudo apt-get install -y coinor-libsymphony-dev" @@
  workdir "opam-repository" @@
  run "git pull origin master" @@
  run "opam admin cache" @@
  run "echo 'wrap-build-commands: []' >> ~/.opamrc" @@
  run "echo 'wrap-install-commands: []' >> ~/.opamrc" @@
  run "echo 'wrap-remove-commands: []' >> ~/.opamrc" @@
  run "opam init -yac %s ." (Intf.Compiler.to_string switch) @@
  run "echo 'archive-mirrors: [\"file:///home/opam/opam-repository/cache\"]' >> /home/opam/.opam/config" @@
  run "opam install -y opam-depext" @@
  Option.map_or ~default:empty (run "%s") (Server_configfile.extra_command conf) @@
  cmd "%s" (Server_configfile.list_command conf)

let with_stderr workdir f =
  Oca_lib.mkdir_p (Server_workdirs.ilogdir workdir) >>= fun () ->
  let logfile = Server_workdirs.ilogfile workdir in
  Lwt_unix.openfile (Fpath.to_string logfile) Unix.[O_WRONLY; O_CREAT; O_APPEND] 0o640 >>= fun stderr ->
  Lwt.finalize (fun () -> f ~stderr) (fun () -> Lwt_unix.close stderr)

let rec parse_maintainers acc = function
  | x::xs ->
      let len = String.length x in
      if len > 2 && Char.equal x.[0] '"' && Char.equal x.[len - 1] '"' then
        parse_maintainers (acc ^ String.sub x 1 (len - 2)) xs
      else
        parse_maintainers acc xs
  | [] ->
      acc

let build_switch ~stderr ~cached conf workdir switch =
  let img_name = get_img_name switch in
  docker_build ~stderr ~cached ~img_name (get_dockerfile ~conf switch) >>= fun () ->
  Server_workdirs.init_base_job ~switch workdir >>= fun () ->
  get_pkgs ~stderr switch >|= fun pkgs ->
  (switch, pkgs)

let get_maintainers ~stderr switch workdir pkgs =
  let img_name = get_img_name switch in
  pkgs |>
  List.uniq ~eq:begin fun x y ->
    let x = Intf.Pkg.name (Intf.Pkg.create ~full_name:x ~instances:[] ~maintainers:[]) in (* TODO: Remove this horror *)
    let y = Intf.Pkg.name (Intf.Pkg.create ~full_name:y ~instances:[] ~maintainers:[]) in (* TODO: Remove this horror *)
    String.equal x y
  end |>
  Lwt_list.iter_s begin fun full_name ->
    let pkg = Intf.Pkg.name (Intf.Pkg.create ~full_name ~instances:[] ~maintainers:[]) in (* TODO: Remove this horror *)
    docker_run_to_str ~stderr ~img_name ["opam";"show";"-f";"maintainer:";pkg] >>= fun maintainers ->
    let maintainers = parse_maintainers "" maintainers in
    let file = Server_workdirs.tmpmaintainersfile ~pkg workdir in
    Lwt_io.with_file ~mode:Lwt_io.Output (Fpath.to_string file) (fun c -> Lwt_io.write c maintainers)
  end

let set_git_hash ~stderr switch conf =
  let img_name = get_img_name switch in
  docker_run_to_str ~stderr ~img_name ["git";"rev-parse";"HEAD"] >>= function
  | [hash] -> Server_configfile.set_opam_repo_commit_hash conf hash
  | _ -> Server_configfile.set_opam_repo_commit_hash conf "[internal failure]" (* TODO: fix *)

let move_tmpdirs_to_final ~stderr workdir =
  let logdir = Server_workdirs.logdir workdir in
  let tmplogdir = Server_workdirs.tmplogdir workdir in
  let maintainersdir = Server_workdirs.maintainersdir workdir in
  let tmpmaintainersdir = Server_workdirs.tmpmaintainersdir workdir in
  (* TODO: replace by Oca_lib.rm_rf *)
  Oca_lib.exec ~stdin:`Close ~stdout:stderr ~stderr ["rm";"-rf";Fpath.to_string logdir] >>= fun () ->
  Lwt_unix.rename (Fpath.to_string tmplogdir) (Fpath.to_string logdir) >>= fun () ->
  (* TODO: replace by Oca_lib.rm_rf *)
  Oca_lib.exec ~stdin:`Close ~stdout:stderr ~stderr ["rm";"-rf";Fpath.to_string maintainersdir] >>= fun () ->
  Lwt_unix.rename (Fpath.to_string tmpmaintainersdir) (Fpath.to_string maintainersdir)

let run_locked = ref false

let run ~on_finished ~conf workdir =
  let switches = Option.get_exn (Server_configfile.ocaml_switches conf) in
  if !run_locked then
    failwith "operation locked";
  run_locked := true;
  Lwt.async begin fun () -> Lwt.finalize begin fun () ->
    with_stderr workdir begin fun ~stderr ->
      Server_workdirs.init_base_jobs ~stderr workdir >>= fun () ->
      begin match switches with
      | switch::switches ->
          build_switch ~stderr ~cached:false conf workdir switch >>= fun hd_pkgs ->
          Lwt_list.map_s (build_switch ~stderr ~cached:true conf workdir) switches >>= fun tl_pkgs ->
          let jobs, pkgs =
            List.fold_left begin fun (jobs, pkgs_acc) (switch, pkgs) ->
              (List.map (run_job ~stderr ~switch workdir) pkgs @ jobs, pkgs @ pkgs_acc)
            end ([], []) (hd_pkgs :: tl_pkgs)
          in
          Lwt.join jobs >>= fun () ->
          set_git_hash ~stderr switch conf >>= fun () ->
          get_maintainers ~stderr switch workdir pkgs
      | [] ->
          Lwt.return_unit
      end >>= fun () ->
      move_tmpdirs_to_final ~stderr workdir >|= fun () ->
      on_finished workdir
    end
  end (fun () -> run_locked := false; Lwt.return_unit) end;
  Lwt.return_unit
