open Lwt.Infix

type t = Server_workdirs.t
type task = unit -> unit Lwt.t

module Pkg = Pkg

(* TODO: Deduplicate with Server.tcp_server *)
let tcp_server port callback =
  Cohttp_lwt_unix.Server.create
    ~on_exn:(fun _ -> ())
    ~mode:(`TCP (`Port port))
    (Cohttp_lwt_unix.Server.make ~callback ())

let start ~on_finished conf workdir =
  let port = Server_configfile.admin_port conf in
  let callback = Admin.callback ~on_finished workdir in
  Admin.create_admin_key workdir >|= fun () ->
  (workdir, fun () -> tcp_server port callback)
