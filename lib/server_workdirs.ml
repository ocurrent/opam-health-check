open Lwt.Infix

type t = Fpath.t

let (/) path file =
  if not (Oca_lib.is_valid_filename file) then
    failwith "Wrong filename";
  Fpath.(/) path file

let (+) = Fpath.(+)

let create ~workdir = Fpath.v workdir

let keysdir workdir = workdir/"keys"
let keyfile ~username workdir = keysdir workdir/username+"key"

let logdir workdir = workdir/"logs"
let tmplogdir workdir = workdir/"tmplogs"

let ilogdir workdir = workdir/"ilogs"
let switchilogdir ~switch workdir = ilogdir workdir/switch
let ilogfile ~switch workdir = switchilogdir ~switch workdir/Printf.sprintf "%.f" (Unix.time ())

let switchlogdir ~switch workdir = logdir workdir/switch
let gooddir ~switch workdir = switchlogdir ~switch workdir/"good"
let baddir ~switch workdir = switchlogdir ~switch workdir/"bad"

let tmpswitchlogdir ~switch workdir = tmplogdir workdir/switch
let tmpgooddir ~switch workdir = tmpswitchlogdir ~switch workdir/"good"
let tmpbaddir ~switch workdir = tmpswitchlogdir ~switch workdir/"bad"
let tmplogfile ~pkg ~switch workdir = tmpswitchlogdir ~switch workdir/pkg

let tmpgoodlog ~pkg ~switch workdir = tmpgooddir ~switch workdir/pkg
let tmpbadlog ~pkg ~switch workdir = tmpbaddir ~switch workdir/pkg

let configfile workdir = workdir/"config.yaml"
let file_from_logdir ~file workdir =
  let file = Fpath.v file in
  let file = Fpath.segs file in
  List.fold_left (/) (logdir workdir) file

let init_base workdir =
  Oca_lib.mkdir_p (keysdir workdir) >>= fun () ->
  Oca_lib.mkdir_p (logdir workdir) >>= fun () ->
  Oca_lib.mkdir_p (ilogdir workdir)

let init_base_job ~switch ~stderr workdir =
  Oca_lib.mkdir_p (gooddir ~switch workdir) >>= fun () ->
  Oca_lib.mkdir_p (baddir ~switch workdir) >>= fun () ->
  Oca_lib.exec ~stdin:`Close ~stdout:stderr ~stderr ["rm";"-rf";Fpath.to_string (tmpswitchlogdir ~switch workdir)] >>= fun () ->
  Oca_lib.mkdir_p (tmpgooddir ~switch workdir) >>= fun () ->
  Oca_lib.mkdir_p (tmpbaddir ~switch workdir)
