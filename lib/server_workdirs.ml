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

let tmpdir workdir = workdir/"tmp"

let logdir ~old workdir = workdir/(if old then "old-logs" else "logs")
let tmplogdir workdir = tmpdir workdir/"logs"

let ilogdir workdir = workdir/"ilogs"
let ilogfile workdir = ilogdir workdir/Printf.sprintf "%.0f" (Unix.time ())

let switchlogdir ~old ~switch workdir = logdir ~old workdir/Intf.Compiler.to_string switch
let gooddir ~old ~switch workdir = switchlogdir ~old ~switch workdir/"good"
let partialdir ~old ~switch workdir = switchlogdir ~old ~switch workdir/"partial"
let baddir ~old ~switch workdir = switchlogdir ~old ~switch workdir/"bad"
let notavailabledir ~old ~switch workdir = switchlogdir ~old ~switch workdir/"not-available"
let internalfailuredir ~old ~switch workdir = switchlogdir ~old ~switch workdir/"internal-failure"

let tmpswitchlogdir ~switch workdir = tmplogdir workdir/Intf.Compiler.to_string switch
let tmpgooddir ~switch workdir = tmpswitchlogdir ~switch workdir/"good"
let tmppartialdir ~switch workdir = tmpswitchlogdir ~switch workdir/"partial"
let tmpbaddir ~switch workdir = tmpswitchlogdir ~switch workdir/"bad"
let tmpnotavailabledir ~switch workdir = tmpswitchlogdir ~switch workdir/"not-available"
let tmpinternalfailuredir ~switch workdir = tmpswitchlogdir ~switch workdir/"internal-failure"
let tmplogfile ~pkg ~switch workdir = tmpswitchlogdir ~switch workdir/pkg

let tmpgoodlog ~pkg ~switch workdir = tmpgooddir ~switch workdir/pkg
let tmppartiallog ~pkg ~switch workdir = tmppartialdir ~switch workdir/pkg
let tmpbadlog ~pkg ~switch workdir = tmpbaddir ~switch workdir/pkg
let tmpnotavailablelog ~pkg ~switch workdir = tmpnotavailabledir ~switch workdir/pkg
let tmpinternalfailurelog ~pkg ~switch workdir = tmpinternalfailuredir ~switch workdir/pkg

let metadatadir workdir = workdir/"metadata"
let maintainersdir workdir = metadatadir workdir/"maintainers"
let maintainersfile ~pkg workdir = maintainersdir workdir/pkg

let tmpmaintainersdir workdir = tmpdir workdir/"maintainers"
let tmpmaintainersfile ~pkg workdir = tmpmaintainersdir workdir/pkg

let configfile workdir = workdir/"config.yaml"
let file_from_logdir ~old ~file workdir =
  let file = Fpath.v file in
  let file = Fpath.segs file in
  List.fold_left (/) (logdir ~old workdir) file

let init_base workdir =
  Oca_lib.mkdir_p (keysdir workdir) >>= fun () ->
  Oca_lib.mkdir_p (logdir ~old:false workdir) >>= fun () ->
  Oca_lib.mkdir_p (logdir ~old:true workdir) >>= fun () ->
  Oca_lib.mkdir_p (ilogdir workdir) >>= fun () ->
  Oca_lib.mkdir_p (maintainersdir workdir)

let init_base_jobs ~stderr workdir =
  Oca_lib.exec ~stdin:`Close ~stdout:stderr ~stderr ["rm";"-rf";Fpath.to_string (tmpdir workdir)] >>= fun () ->
  Oca_lib.mkdir_p (tmplogdir workdir) >>= fun () ->
  Oca_lib.mkdir_p (tmpmaintainersdir workdir)

let init_base_job ~switch workdir =
  Oca_lib.mkdir_p (tmpgooddir ~switch workdir) >>= fun () ->
  Oca_lib.mkdir_p (tmppartialdir ~switch workdir) >>= fun () ->
  Oca_lib.mkdir_p (tmpbaddir ~switch workdir) >>= fun () ->
  Oca_lib.mkdir_p (tmpnotavailabledir ~switch workdir) >>= fun () ->
  Oca_lib.mkdir_p (tmpinternalfailuredir ~switch workdir)
