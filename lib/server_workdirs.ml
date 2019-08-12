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

type logdir = Logdir of float * string * t

let logdir_from_string workdir s =
  match String.split_on_char '-' s with
  | [time; hash] -> Logdir (float_of_string time, hash, workdir)
  | _ -> assert false

let base_logdir workdir = workdir/"logs"
let base_tmpdir workdir = workdir/"tmp"
let new_logdir ~hash ~start_time workdir = Logdir (start_time, hash, workdir)
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

let tmplogdir (Logdir (_, _, workdir) as logdir) = base_tmpdir workdir/get_logdir_name logdir/"logs"

let ilogdir workdir = workdir/"ilogs"
let new_ilogfile ~start_time workdir = ilogdir workdir/Printf.sprintf "%.0f" start_time

let switchlogdir ~switch logdir = get_logdir_path logdir/Intf.Compiler.to_string switch
let gooddir ~switch logdir = switchlogdir ~switch logdir/"good"
let partialdir ~switch logdir = switchlogdir ~switch logdir/"partial"
let baddir ~switch logdir = switchlogdir ~switch logdir/"bad"
let notavailabledir ~switch logdir = switchlogdir ~switch logdir/"not-available"
let internalfailuredir ~switch logdir = switchlogdir ~switch logdir/"internal-failure"

let tmpswitchlogdir ~switch logdir = tmplogdir logdir/Intf.Compiler.to_string switch
let tmpgooddir ~switch logdir = tmpswitchlogdir ~switch logdir/"good"
let tmppartialdir ~switch logdir = tmpswitchlogdir ~switch logdir/"partial"
let tmpbaddir ~switch logdir = tmpswitchlogdir ~switch logdir/"bad"
let tmpnotavailabledir ~switch logdir = tmpswitchlogdir ~switch logdir/"not-available"
let tmpinternalfailuredir ~switch logdir = tmpswitchlogdir ~switch logdir/"internal-failure"
let tmplogfile ~pkg ~switch logdir = tmpswitchlogdir ~switch logdir/pkg

let tmpgoodlog ~pkg ~switch logdir = tmpgooddir ~switch logdir/pkg
let tmppartiallog ~pkg ~switch logdir = tmppartialdir ~switch logdir/pkg
let tmpbadlog ~pkg ~switch logdir = tmpbaddir ~switch logdir/pkg
let tmpnotavailablelog ~pkg ~switch logdir = tmpnotavailabledir ~switch logdir/pkg
let tmpinternalfailurelog ~pkg ~switch logdir = tmpinternalfailuredir ~switch logdir/pkg

let metadatadir workdir = workdir/"metadata"
let maintainersdir workdir = metadatadir workdir/"maintainers"
let maintainersfile ~pkg workdir = maintainersdir workdir/pkg
let revdepsdir workdir = metadatadir workdir/"revdeps"
let revdepsfile ~pkg workdir = revdepsdir workdir/pkg

let tmpmetadatadir (Logdir (_, _, workdir) as logdir) = base_tmpdir workdir/get_logdir_name logdir/"metadata"

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

let init_base_job ~switch logdir =
  Oca_lib.mkdir_p (tmpgooddir ~switch logdir) >>= fun () ->
  Oca_lib.mkdir_p (tmppartialdir ~switch logdir) >>= fun () ->
  Oca_lib.mkdir_p (tmpbaddir ~switch logdir) >>= fun () ->
  Oca_lib.mkdir_p (tmpnotavailabledir ~switch logdir) >>= fun () ->
  Oca_lib.mkdir_p (tmpinternalfailuredir ~switch logdir)

let init_base_jobs ~switches logdir =
  Oca_lib.mkdir_p (tmplogdir logdir) >>= fun () ->
  Oca_lib.mkdir_p (tmpmetadatadir logdir) >>= fun () ->
  Lwt_list.iter_s (fun switch -> init_base_job ~switch logdir) switches
