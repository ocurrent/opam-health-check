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

type logdir = Logdir of float * string * t

let logdir_from_string workdir s =
  match String.split_on_char '-' s with
  | [time; hash] -> Logdir (float_of_string time, hash, workdir)
  | _ -> assert false

let base_logdir workdir = workdir/"logs"
let new_logdir ~hash workdir = Logdir (Unix.time (), hash, workdir)
let logdirs workdir =
  Oca_lib.get_files (base_logdir workdir) >|= fun dirs ->
  let dirs = List.sort (fun x y -> -String.compare x y) dirs in
  List.map (logdir_from_string workdir) dirs

let logdir_equal (Logdir (time1, hash1, workdir1)) (Logdir (time2, hash2, workdir2)) =
  Float.equal time1 time2 &&
  String.equal hash1 hash2 &&
  Fpath.equal workdir1 workdir2

let get_logdir_name (Logdir (time, hash, _)) = Printf.sprintf "%.0f-%s" time hash
let get_logdir_path (Logdir (_, _, workdir) as logdir) = base_logdir workdir/get_logdir_name logdir
let get_logdir_hash (Logdir (_, hash, _)) = hash
let get_logdir_time (Logdir (time, _, _)) = time

let tmplogdir workdir = tmpdir workdir/"logs"

let ilogdir workdir = workdir/"ilogs"
let ilogfile workdir = ilogdir workdir/Printf.sprintf "%.0f" (Unix.time ())

let switchlogdir ~switch logdir = get_logdir_path logdir/Intf.Compiler.to_string switch
let gooddir ~switch logdir = switchlogdir ~switch logdir/"good"
let partialdir ~switch logdir = switchlogdir ~switch logdir/"partial"
let baddir ~switch logdir = switchlogdir ~switch logdir/"bad"
let notavailabledir ~switch logdir = switchlogdir ~switch logdir/"not-available"
let internalfailuredir ~switch logdir = switchlogdir ~switch logdir/"internal-failure"

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
let revdepsdir workdir = metadatadir workdir/"revdeps"
let revdepsfile ~pkg workdir = revdepsdir workdir/pkg

let tmpmetadatadir workdir = tmpdir workdir/"metadata"

let configfile workdir = workdir/"config.yaml"
let file_from_logdir ~file logdir =
  let file = Fpath.v file in
  let file = Fpath.segs file in
  List.fold_left (/) (get_logdir_path logdir) file

let init_base workdir =
  Oca_lib.mkdir_p (keysdir workdir) >>= fun () ->
  Oca_lib.mkdir_p (base_logdir workdir) >>= fun () ->
  Oca_lib.mkdir_p (ilogdir workdir) >>= fun () ->
  Oca_lib.mkdir_p (maintainersdir workdir) >>= fun () ->
  Oca_lib.mkdir_p (revdepsdir workdir)

let init_base_jobs ~stderr workdir =
  Oca_lib.exec ~stdin:`Close ~stdout:stderr ~stderr ["rm";"-rf";Fpath.to_string (tmpdir workdir)] >>= fun () ->
  Oca_lib.mkdir_p (tmplogdir workdir) >>= fun () ->
  Oca_lib.mkdir_p (tmpmetadatadir workdir)

let init_base_job ~switch workdir =
  Oca_lib.mkdir_p (tmpgooddir ~switch workdir) >>= fun () ->
  Oca_lib.mkdir_p (tmppartialdir ~switch workdir) >>= fun () ->
  Oca_lib.mkdir_p (tmpbaddir ~switch workdir) >>= fun () ->
  Oca_lib.mkdir_p (tmpnotavailabledir ~switch workdir) >>= fun () ->
  Oca_lib.mkdir_p (tmpinternalfailuredir ~switch workdir)
