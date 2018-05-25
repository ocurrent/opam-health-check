open Lwt.Infix

type t = string

let (/) = Filename.concat

let create ~workdir = workdir

let keysdir workdir = workdir/"keys"
let keyfile ~username workdir = keysdir workdir/username^".key"

let logdir workdir = workdir/"logs"
let tmplogdir workdir = workdir/"tmplogs"

let ilogdir workdir = workdir/"ilogs"
let switchilogdir ~switch workdir = ilogdir workdir/switch
let ilogfile ~switch workdir = switchilogdir ~switch workdir/Printf.sprintf "%.f" (Unix.time ())

let switchlogdir ~switch workdir = logdir workdir/switch
let gooddir ~switch workdir = switchlogdir ~switch workdir/"good"
let baddir ~switch workdir = switchlogdir ~switch workdir/"bad"
let logfile ~pkg ~switch workdir = switchlogdir ~switch workdir/pkg

let tmpswitchlogdir ~switch workdir = tmplogdir workdir/switch
let tmpgooddir ~switch workdir = tmpswitchlogdir ~switch workdir/"good"
let tmpbaddir ~switch workdir = tmpswitchlogdir ~switch workdir/"bad"

let tmpgoodlog ~pkg ~switch workdir = tmpgooddir ~switch workdir/pkg
let tmpbadlog ~pkg ~switch workdir = tmpbaddir ~switch workdir/pkg

let configfile workdir = workdir/"config.yaml"
let file_from_logdir ~file workdir = logdir workdir/file

let init_base workdir =
  Oca_lib.mkdir_p (keysdir workdir) >>= fun () ->
  Oca_lib.mkdir_p (logdir workdir) >>= fun () ->
  Oca_lib.mkdir_p (ilogdir workdir)

let init_base_job ~switch workdir =
  Oca_lib.mkdir_p (switchilogdir ~switch workdir) >>= fun () ->
  Oca_lib.mkdir_p (gooddir ~switch workdir) >>= fun () ->
  Oca_lib.mkdir_p (baddir ~switch workdir) >>= fun () ->
  Oca_lib.mkdir_p (tmpbaddir ~switch workdir) >>= fun () ->
  Oca_lib.mkdir_p (tmpbaddir ~switch workdir)
