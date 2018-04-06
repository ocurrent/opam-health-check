open Lwt.Infix

let pool = Lwt_pool.create 32 (fun () -> Lwt.return_unit)

let exec_in ~stdout str cmd =
  let stdin, fd = Lwt_unix.pipe_out () in
  let stdin = `FD_move stdin in
  let fd = Lwt_io.of_fd ~mode:Lwt_io.Output fd in
  Lwt_io.write_line fd str >>= fun () ->
  Lwt_io.close fd >>= fun () ->
  Lwt_process.exec ~stdin ~stdout ("", Array.of_list cmd) >>= function
  | Unix.WEXITED 0 ->
      Lwt.return (Ok ())
  | _ ->
      let cmd = String.concat " " cmd in
      Lwt_io.write_line Lwt_io.stderr ("Command '"^cmd^"' failed.") >>= fun () ->
      Lwt.return (Error ())

let docker_build ~stdout args dockerfile =
  exec_in
    ~stdout
    (Dockerfile.string_of_t dockerfile)
    ("docker"::"build"::args@["-"])

let get_pkgs ~base_img ~img_name ~logdir ~switch =
  let dockerfile =
    let open Dockerfile in
    from base_img @@
    run "sudo apt-get update" @@
    run "git checkout 632bc2eed" @@
    run "git pull origin 2.0.0" @@
    run "opam update" @@
    run "opam admin cache" @@
    run "echo 'archive-mirrors: [\"file:///home/opam/opam-repository/cache\"]' >> /home/opam/.opam/config" @@
    run "opam switch create -y %s" switch @@
    run "opam install -y opam-depext" @@
    cmd "opam list --installable --available --short --all-versions"
  in
  docker_build ~stdout:`Keep ["-t"; img_name] dockerfile >>= fun _ ->
  Lwt_io.write_line Lwt_io.stdout "Getting packages list..." >>= fun () ->
  Lwt_process.pread ("", [|"docker"; "run"; img_name|]) >|=
  String.split_on_char '\n'

let rec get_jobs ~img_name ~logdir ~gooddir ~baddir jobs = function
  | [] ->
      Lwt.return jobs
  | pkg::pkgs ->
      let dockerfile =
        let open Dockerfile in
        from img_name @@
        run "opam pin add -yn --dev ocamlfind" @@
        run "opam depext -ivy %s" pkg
      in
      let job =
        Lwt_pool.use pool begin fun () ->
          let goodlog = Filename.concat gooddir pkg in
          let badlog = Filename.concat baddir pkg in
          Lwt_unix.file_exists goodlog >>= fun goodlog_exists ->
          Lwt_unix.file_exists badlog >>= fun badlog_exists ->
          if goodlog_exists || badlog_exists then begin
            Lwt_io.write_line Lwt_io.stdout (pkg^" has already been checked. Skipping...")
          end else begin
            Lwt_io.write_line Lwt_io.stdout ("Checking "^pkg^"...") >>= fun () ->
            let logfile = Filename.concat logdir pkg in
            Lwt_unix.openfile logfile [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o640 >>= fun stdout ->
            let stdout = Lwt_unix.unix_file_descr stdout in
            let stdout = `FD_move stdout in
            docker_build ~stdout ["-t"; "base-opam-check-all"] dockerfile >>= begin function
            | Ok () -> Lwt_unix.rename logfile goodlog
            | Error () -> Lwt_unix.rename logfile badlog
            end >>= fun () ->
            Lwt_process.exec ("", [|"docker"; "image"; "prune"; "-f"|]) >>= fun _ ->
            Lwt_process.exec ("", [|"docker"; "container"; "prune"; "-f"|]) >>= fun _ ->
            Lwt.return_unit
          end
        end
      in
      get_jobs ~img_name ~logdir ~gooddir ~baddir (job :: jobs) pkgs

let () =
  match Sys.argv with
  | [|_; base_img; img_name; logdir; switch|] ->
      let gooddir = Filename.concat logdir "good" in
      let baddir = Filename.concat logdir "bad" in
      Lwt_main.run begin
        Lwt_process.exec ("", [|"mkdir"; "-p"; gooddir|]) >>= fun _ ->
        Lwt_process.exec ("", [|"mkdir"; "-p"; baddir|]) >>= fun _ ->
        get_pkgs ~base_img ~img_name ~logdir ~switch >>=
        get_jobs ~img_name ~logdir ~gooddir ~baddir [] >>=
        Lwt.join
      end
  | _ ->
      prerr_endline "Read the code and try again";
      exit 1
