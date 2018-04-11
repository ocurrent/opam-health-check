open Containers
open Lwt.Infix

let serv_file ~content_type ~logdir file =
  let headers = Cohttp.Header.init_with "Content-Type" content_type in
  let fname = Filename.concat logdir file in
  Cohttp_lwt_unix.Server.respond_file ~headers ~fname ()

let callback logdir _conn req _body =
  match Uri.path (Cohttp.Request.uri req) with
  | "" | "/" -> serv_file ~content_type:"text/html" ~logdir "index.html"
  | file ->
      if String.mem ~sub:".." file then
        Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:"You bastard !" ()
      else
        serv_file ~content_type:"text/plain" ~logdir file

let () =
  match Sys.argv with
  | [|_; logdir|] ->
      let callback = callback logdir in
      Lwt_main.run begin
        Cohttp_lwt_unix.Server.create
          ~on_exn:(fun _ -> ())
          ~mode:(`TCP (`Port 8080))
          (Cohttp_lwt_unix.Server.make ~callback ())
      end
  | _ ->
      prerr_endline "Read the code and try again";
      exit 1
