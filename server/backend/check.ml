open Lwt.Infix

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

let get_img_name ~conf switch =
  let switch = Intf.Compiler.to_string switch in
  let switch =
    let rec normalize_docker_tag_name = function
      | ('a'..'z' | 'A'..'Z' | '0'..'9' | '-' | '.') as c::cs -> c::normalize_docker_tag_name cs
      | '_'::'_'::_ -> assert false
      | _::cs -> '_'::'_'::normalize_docker_tag_name cs
      | [] -> []
    in
    String.of_list (normalize_docker_tag_name (String.to_list switch))
  in
  let server_name = Server_configfile.name conf in
  "opam-check-all-"^server_name^"-"^switch

let get_pkgs ~conf ~stderr switch =
  let img_name = get_img_name ~conf switch in
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

let run_job ~conf ~pool ~stderr ~switch ~num workdir pkg =
  let img_name = get_img_name ~conf switch in
  Lwt_pool.use pool begin fun () ->
    Oca_lib.write_line_unix stderr ("["^num^"] Checking "^pkg^" on "^Intf.Compiler.to_string switch^"...") >>= fun () ->
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
    | Oca_lib.Process_failure 31 ->
        is_partial_failure logfile >>= begin function
        | true -> Lwt_unix.rename (Fpath.to_string logfile) (Fpath.to_string (Server_workdirs.tmppartiallog ~pkg ~switch workdir))
        | false -> Lwt_unix.rename (Fpath.to_string logfile) (Fpath.to_string (Server_workdirs.tmpbadlog ~pkg ~switch workdir))
        end
    | Oca_lib.Process_failure 20 ->
        Lwt_unix.rename (Fpath.to_string logfile) (Fpath.to_string (Server_workdirs.tmpnotavailablelog ~pkg ~switch workdir))
    | Oca_lib.Process_failure _ | Oca_lib.Internal_failure ->
        Lwt_unix.rename (Fpath.to_string logfile) (Fpath.to_string (Server_workdirs.tmpinternalfailurelog ~pkg ~switch workdir))
    | e ->
        Lwt.fail e
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
  run "opam init -ya --bare ." @@
  run "opam repository add --dont-select beta git://github.com/ocaml/ocaml-beta-repository.git" @@
  run "opam switch create --repositories=default,beta %s" (Intf.Compiler.to_string switch) @@
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
  let img_name = get_img_name ~conf switch in
  docker_build ~stderr ~cached ~img_name (get_dockerfile ~conf switch) >>= fun () ->
  Server_workdirs.init_base_job ~switch workdir >>= fun () ->
  get_pkgs ~conf ~stderr switch >|= fun pkgs ->
  (switch, pkgs)

module Pkg_set = Set.Make (String)

let get_maintainers ~conf ~pool ~jobs ~stderr switch workdir pkgs =
  let img_name = get_img_name ~conf switch in
  Pkg_set.fold begin fun pkg jobs ->
    Lwt_pool.use pool begin fun () ->
      Oca_lib.write_line_unix stderr ("Getting the list of maintainers for "^pkg^"...") >>= fun () ->
      docker_run_to_str ~stderr ~img_name ["opam";"show";"-f";"maintainer:";pkg] >>= fun maintainers ->
      let maintainers = parse_maintainers "" maintainers in
      let file = Server_workdirs.tmpmaintainersfile ~pkg workdir in
      Lwt_io.with_file ~mode:Lwt_io.Output (Fpath.to_string file) (fun c -> Lwt_io.write c maintainers)
    end :: jobs
  end pkgs jobs

let set_git_hash ~pool ~stderr switch conf =
  let img_name = get_img_name ~conf switch in
  Lwt_pool.use pool begin fun () ->
    Oca_lib.write_line_unix stderr "Getting current git hash..." >>= fun () ->
    docker_run_to_str ~stderr ~img_name ["git";"rev-parse";"HEAD"] >>= function
    | [hash] ->
        Oca_lib.write_line_unix stderr ("Current git hash: "^hash) >>= fun () ->
        Server_configfile.set_opam_repo_commit_hash conf hash
    | s ->
        let s = String.unlines s in
        Oca_lib.write_line_unix stderr ("Error: cannot parse git hash. Got:\n"^s) >>= fun () ->
        Lwt.fail_with "Something went wrong. See internal log"
  end

let move_tmpdirs_to_final ~stderr workdir =
  let logdir = Server_workdirs.logdir ~old:false workdir in
  let oldlogdir = Server_workdirs.logdir ~old:true workdir in
  let tmplogdir = Server_workdirs.tmplogdir workdir in
  let maintainersdir = Server_workdirs.maintainersdir workdir in
  let tmpmaintainersdir = Server_workdirs.tmpmaintainersdir workdir in
  (* TODO: replace by Oca_lib.rm_rf *)
  Oca_lib.exec ~stdin:`Close ~stdout:stderr ~stderr ["rm";"-rf";Fpath.to_string oldlogdir] >>= fun () ->
  Lwt_unix.rename (Fpath.to_string logdir) (Fpath.to_string oldlogdir) >>= fun () ->
  Lwt_unix.rename (Fpath.to_string tmplogdir) (Fpath.to_string logdir) >>= fun () ->
  (* TODO: replace by Oca_lib.rm_rf *)
  Oca_lib.exec ~stdin:`Close ~stdout:stderr ~stderr ["rm";"-rf";Fpath.to_string maintainersdir] >>= fun () ->
  Lwt_unix.rename (Fpath.to_string tmpmaintainersdir) (Fpath.to_string maintainersdir)

let run_and_get_pkgs ~conf ~pool ~stderr workdir pkgs =
  let len_suffix = "/"^string_of_int (List.fold_left (fun n (_, pkgs) -> n + List.length pkgs) 0 pkgs) in
  List.fold_left begin fun (i, jobs, pkgs_set) (switch, pkgs) ->
    List.fold_left begin fun (i, jobs, pkgs_set) full_name ->
      let num = string_of_int i^len_suffix in
      let job = run_job ~conf ~pool ~stderr ~switch ~num workdir full_name in
      let pkg = Intf.Pkg.name (Intf.Pkg.create ~full_name ~instances:[] ~maintainers:[]) in (* TODO: Remove this horror *)
      (succ i, job :: jobs, Pkg_set.add pkg pkgs_set)
    end (succ i, jobs, pkgs_set) pkgs
  end (0, [], Pkg_set.empty) pkgs

let run_locked = ref false

let run ~on_finished ~is_retry ~conf workdir =
  let switches = Option.get_exn (Server_configfile.ocaml_switches conf) in
  if !run_locked then
    failwith "operation locked";
  run_locked := true;
  Lwt.async begin fun () -> Lwt.finalize begin fun () ->
    let start_time = Unix.time () in
    with_stderr workdir begin fun ~stderr ->
      Server_workdirs.init_base_jobs ~stderr workdir >>= fun () ->
      begin match switches with
      | switch::switches ->
          build_switch ~stderr ~cached:is_retry conf workdir switch >>= fun hd_pkgs ->
          let pool = Lwt_pool.create 32 (fun () -> Lwt.return_unit) in
          Lwt_list.map_s (build_switch ~stderr ~cached:true conf workdir) switches >>= fun tl_pkgs ->
          let (_, jobs, pkgs) = run_and_get_pkgs ~conf ~pool ~stderr workdir (hd_pkgs :: tl_pkgs) in
          let jobs = set_git_hash ~pool ~stderr switch conf :: jobs in
          let jobs = get_maintainers ~conf ~pool ~jobs ~stderr switch workdir pkgs in
          Lwt.join jobs
      | [] ->
          Lwt.return_unit
      end >>= fun () ->
      Oca_lib.write_line_unix stderr "Finishing up..." >>= fun () ->
      move_tmpdirs_to_final ~stderr workdir >>= fun () ->
      on_finished workdir;
      let end_time = Unix.time () in
      Oca_lib.write_line_unix stderr ("Done. Operation took: "^string_of_float (end_time -. start_time)^" seconds")
    end
  end (fun () -> run_locked := false; Lwt.return_unit) end;
  Lwt.return_unit
