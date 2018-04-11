open Containers
open Lwt.Infix

let serv_file ~logdir file =
  let file = Filename.concat logdir file in
  Lwt_io.with_file ~mode:Lwt_io.Input file begin fun file ->
    Lwt_io.read file >>= fun body ->
    Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body ()
  end

let callback logdir _conn req _body =
  match Uri.path (Cohttp.Request.uri req) with
  | "" | "/" -> serv_file ~logdir "index.html"
  | file ->
      if String.mem ~sub:".." file then
        Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:"You bastard !" ()
      else
        serv_file ~logdir file

let () =
  match Sys.argv with
  | [|_; logdir|] ->
      let callback = callback logdir in
      Lwt_main.run begin
        Cohttp_lwt_unix.Server.create
          ~mode:(`TCP (`Port 8080))
          (Cohttp_lwt_unix.Server.make ~callback ())
      end
  | _ ->
      prerr_endline "Read the code and try again";
      exit 1
