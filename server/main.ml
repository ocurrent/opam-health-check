module Server = Oca_server.Server.Make (Backend)

let main debug workdir cap_file = Lwt_main.run (Server.main ~debug ~cap_file ~workdir)

(* Command-line parsing *)

module Arg = Cmdliner.Arg
module Term = Cmdliner.Term

let debug = Arg.(value @@ flag @@ info ["debug"])

let workdir =
  Arg.(required @@ pos 0 Arg.(some string) None @@ info ~docv:"WORKDIR" [])

let connect_addr =
  Arg.(required @@
  opt (some file) None @@
  info
    ~doc:"Path of ocluster.cap from ocluster-admin"
    ~docv:"ADDR"
    ["c"; "connect"])

let term = Term.(const main $ debug $ workdir $ connect_addr)

let info =
  Term.info
    ~doc:"A server to check for broken opam packages."
    ~man:[`P "This program takes a work directory where every files created \
              are stored. This includes logs, config file and user private \
              keys."]
    ~version:Config.version
    Config.name

let () = Term.exit (Term.eval (term, info))
