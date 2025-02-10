open Lwt.Syntax

type t = Fpath.t

let (/) path file =
  if not (Oca_lib.is_valid_filename file) then
    failwith "Wrong filename";
  Fpath.(/) path file

let (+) = Fpath.(+)
let (//) = Fpath.(//)

let create ~cwd ~workdir = Fpath.normalize (Fpath.v cwd // Fpath.v workdir)

let keysdir workdir = workdir/"keys"
let keyfile ~username workdir = keysdir workdir/username+"key"

type logdir_files = string list

type logdir_ty = Uncompressed | Compressed
type logdir = Logdir of (logdir_ty * float * string * t * logdir_files)
(* TODO: differenciate logdir and tmplogdir *)

let base_logdir workdir = workdir/"logs"
let base_tmpdir workdir = workdir/"tmp"

let new_logdir ~compressed ~hash ~start_time workdir =
  let ty = if compressed then Compressed else Uncompressed in
  Logdir (ty, start_time, hash, workdir, [])

let logdirs workdir =
  let base_logdir = base_logdir workdir in
  let* dirs = Oca_lib.get_files base_logdir in
  let dirs = List.sort (fun x y -> -String.compare x y) dirs in
  let pool = Lwt_pool.create 32 (fun () -> Lwt.return_unit) in
  Lwt_list.map_p (fun dir ->
    match String.split_on_char '-' dir with
    | [time; hash] ->
        let logdir = base_logdir/dir in
        begin match String.split_on_char '.' hash with
        | [hash] ->
            let+ files = Lwt_pool.use pool (fun () -> Oca_lib.scan_dir logdir) in
            Logdir (Uncompressed, float_of_string time, hash, workdir, files)
        | [hash; "txz"] ->
            let+ files = Lwt_pool.use pool (fun () -> Oca_lib.scan_tpxz_archive logdir) in
            Logdir (Compressed, float_of_string time, hash, workdir, files)
        | _ -> assert false
        end
    | _ -> assert false
  ) dirs

let logdir_ty_equal ty1 ty2 = match ty1, ty2 with
  | Uncompressed, Uncompressed
  | Compressed, Compressed -> true
  | Uncompressed, _
  | Compressed, _ -> false

let logdir_equal (Logdir (ty1, time1, hash1, workdir1, _files1)) (Logdir (ty2, time2, hash2, workdir2, _files2)) =
  logdir_ty_equal ty1 ty2 &&
  Float.equal time1 time2 &&
  String.equal hash1 hash2 &&
  Fpath.equal workdir1 workdir2

let get_logdir_name (Logdir (_, time, hash, _, _)) = Printf.sprintf "%.0f-%s" time hash
let get_logdir_hash (Logdir (_, _, hash, _, _)) = hash
let get_logdir_time (Logdir (_, time, _, _, _)) = time

let get_files ~name ~switch (Logdir (_, _, _, _, files)) =
  let switch = Intf.Compiler.to_string switch in
  List.filter_map (fun file ->
    match String.split_on_char '/' file with
    | [_switch; _name; ""] -> None
    | [switch'; name'; pkg] when String.equal switch switch' && String.equal name name' -> Some pkg
    | _ -> None
  ) files

let goodfiles = get_files ~name:"good"
let partialfiles = get_files ~name:"partial"
let badfiles = get_files ~name:"bad"
let notavailablefiles = get_files ~name:"not-available"
let internalfailurefiles = get_files ~name:"internal-failure"

let logdir_get_compilers (Logdir (_, _, _, _, files)) =
  List.filter_map (fun file ->
    match String.split_on_char '/' file with
    | [switch; ""] -> Some (Intf.Compiler.from_string switch)
    | _ -> None
  ) files

let logdir_get_content ~comp ~state ~pkg = function
  | Logdir (Uncompressed, _, _, workdir, _) as logdir ->
      let comp = Intf.Compiler.to_string comp in
      let state = Intf.State.to_string state in
      let file = base_logdir workdir/get_logdir_name logdir/comp/state/pkg in
      Lwt_io.with_file ~mode:Lwt_io.Input (Fpath.to_string file) (Lwt_io.read ?count:None)
  | Logdir (Compressed, _, _, workdir, _) as logdir ->
      let archive = base_logdir workdir/get_logdir_name logdir+"txz" in
      let comp = Intf.Compiler.to_string comp in
      let state = Intf.State.to_string state in
      let file = comp^"/"^state^"/"^pkg in
      Oca_lib.random_access_tpxz_archive ~file archive

let logdir_search ~switch ~regexp = function
  | Logdir (Uncompressed, _, _, workdir, _) as logdir ->
      let cwd = base_logdir workdir/get_logdir_name logdir in
      Oca_lib.ugrep_dir ~switch ~regexp ~cwd
  | Logdir (Compressed, _, _, workdir, _) as logdir ->
      let archive = base_logdir workdir/get_logdir_name logdir+"txz" in
      Oca_lib.ugrep_tpxz ~switch ~regexp ~archive

let tmpdir (Logdir (_, _, _, workdir, _) as logdir) = base_tmpdir workdir/get_logdir_name logdir

let tmplogdir logdir = tmpdir logdir/"logs"
let tmpswitchlogdir ~switch logdir = tmplogdir logdir/Intf.Compiler.to_string switch

let logdir_move ~switches (Logdir (ty, _, _, workdir, _) as logdir) = match ty with
  | Compressed ->
      let cwd = tmplogdir logdir in
      let directories = List.map Intf.Compiler.to_string switches in
      let archive = base_logdir workdir/get_logdir_name logdir+"txz" in
      let* () = Oca_lib.compress_tpxz_archive ~cwd ~directories archive in
      Oca_lib.rm_rf cwd
  | Uncompressed ->
      let tmplogdir = tmplogdir logdir in
      let logdir = base_logdir workdir/get_logdir_name logdir in
      Lwt_unix.rename (Fpath.to_string tmplogdir) (Fpath.to_string logdir)

let ilogdir workdir = workdir/"ilogs"
let new_ilogfile ~start_time workdir = ilogdir workdir/Printf.sprintf "%.0f" start_time

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
let opamsdir workdir = metadatadir workdir/"opams"
let opamfile ~pkg workdir = opamsdir workdir/pkg
let revdepsdir workdir = metadatadir workdir/"revdeps"
let revdepsfile ~pkg workdir = revdepsdir workdir/pkg

let tmpmetadatadir (Logdir (_, _, _, workdir, _) as logdir) = base_tmpdir workdir/get_logdir_name logdir/"metadata"
let tmpopamsdir logdir = tmpmetadatadir logdir/"opams"
let tmpopamfile ~pkg logdir = tmpopamsdir logdir/pkg
let tmprevdepsdir logdir = tmpmetadatadir logdir/"revdeps"
let tmprevdepsfile ~pkg logdir = tmprevdepsdir logdir/pkg

let configfile workdir = workdir/"config.yaml"

let init_base workdir =
  let* () = Oca_lib.mkdir_p (keysdir workdir) in
  let* () = Oca_lib.mkdir_p (base_logdir workdir) in
  let* () = Oca_lib.mkdir_p (ilogdir workdir) in
  let* () = Oca_lib.mkdir_p (opamsdir workdir) in
  Oca_lib.mkdir_p (revdepsdir workdir)

let init_base_job ~switch logdir =
  let switch = Intf.Switch.name switch in
  let* () = Oca_lib.mkdir_p (tmpgooddir ~switch logdir) in
  let* () = Oca_lib.mkdir_p (tmppartialdir ~switch logdir) in
  let* () = Oca_lib.mkdir_p (tmpbaddir ~switch logdir) in
  let* () = Oca_lib.mkdir_p (tmpnotavailabledir ~switch logdir) in
  Oca_lib.mkdir_p (tmpinternalfailuredir ~switch logdir)

let init_base_jobs ~switches logdir =
  let* () = Oca_lib.mkdir_p (tmplogdir logdir) in
  let* () = Oca_lib.mkdir_p (tmpmetadatadir logdir) in
  let* () = Oca_lib.mkdir_p (tmprevdepsdir logdir) in
  let* () = Oca_lib.mkdir_p (tmpopamsdir logdir) in
  Lwt_list.iter_s (fun switch -> init_base_job ~switch logdir) switches
