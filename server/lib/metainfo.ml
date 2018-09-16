open Lwt.Infix

module Store = Git.Mem.Store(Digestif.SHA1)
module Sync = Git_unix.Sync(Store)

let skip_store_error = function
  | Ok x -> Lwt.return x
  | Error err -> Lwt.fail_with (Format.sprintf "Store error %a" Store.pp_error err)

let skip_sync_error = function
  | Ok x -> Lwt.return x
  | Error err -> Lwt.fail_with (Format.sprintf "Sync error %a" Sync.pp_error err)

let get_pkgsinfo' () =
  Store.v (Fpath.v Filename.current_dir_name) >>= skip_store_error >>= fun store ->
  let reference = Git.Reference.of_string "refs/heads/index" in
  Sync.clone_ext store ~reference (Uri.of_string "git://github.com/ocaml/obi-logs.git") >>= skip_sync_error >>= fun repo ->
  Store.Ref.write store reference (Store.Reference.Hash repo) >>= skip_store_error >>= fun () ->
  Store.Ref.write store Store.Reference.head (Store.Reference.Ref reference) >>= skip_store_error >>= fun () ->
  Store.Ref.resolve store Store.Reference.head >>= skip_store_error >>= fun hash ->
  Store.read store hash >>= skip_store_error >>= function
  | Store.Value.Commit commit ->
      let aux acc ?name ~length:_ _ value = match acc, name with
      | Some _, _ -> Lwt.return acc
      | None, None -> Lwt.return_none
      | None, Some name when Fpath.equal name (Fpath.v "./index.sxp") -> Lwt.return (Some value)
      | None, Some _ -> Lwt.return_none
      in
      Store.fold store aux ~path:Fpath.(v ".") None (Store.Value.Commit.tree commit) >>= begin function
      | Some (Store.Value.Blob blob) ->
          let {Obi.Index.version; packages} = Obi.Index.t_of_sexp (Sexplib.Sexp.of_string (Store.Value.Blob.to_string blob)) in
          if version <> Obi.Index.current_version then
            failwith "Version missmatch. Please recompile";
          Lwt.return packages
      | Some _ -> Lwt.fail_with "Something when wrong with the metadata repository"
      | None -> Lwt.fail_with "Metadata file not found"
      end
  | _ -> assert false

(* Retry every minutes if something has gone wrong *)
let rec get_pkgsinfo () =
  Lwt.catch get_pkgsinfo' (fun _ -> Lwt_unix.sleep 60. >>= get_pkgsinfo)
