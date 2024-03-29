module Server = Oca_server.Server.Make (Backend)

let main debug workdir cap_file = Lwt_main.run (Server.main ~debug ~cap_file ~workdir)

(* Command-line parsing *)

module Arg = Cmdliner.Arg
module Cmd = Cmdliner.Cmd
module Term = Cmdliner.Term

let ( $ ) = Term.( $ )
let ( & ) = Arg.( & )

let debug = Arg.value & Arg.flag & Arg.info ["debug"]

let workdir =
  Arg.required & Arg.pos 0 (Arg.some Arg.string) None & Arg.info ~docv:"WORKDIR" []

let connect_addr =
  Arg.required &
  Arg.opt (Arg.some Arg.file) None &
  Arg.info
    ~doc:"Path of ocluster.cap from ocluster-admin"
    ~docv:"ADDR"
    ["c"; "connect"]

let term = Term.const main $ debug $ workdir $ connect_addr

let info =
  Cmd.info
    ~doc:"A server to check for broken opam packages."
    ~man:[`P "This program takes a work directory where every files created \
              are stored. This includes logs, config file and user private \
              keys."]
    ~version:Config.version
    Config.name

let () =
  Memtrace.trace_if_requested ~context:"opam-health-check" ();
  exit (Cmd.eval (Cmd.v info term))
