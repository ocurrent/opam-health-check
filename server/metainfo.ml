open Lwt.Infix

module Sexp = Sexplib.Sexp
module Store = Git.Mem.Store(Digestif.SHA1)
module Sync = Git_unix.Sync(Store)

type t = {
  maintainers : string list;
}

let extract_maintainer = function
  | Sexp.Atom x -> x
  | Sexp.List _ -> failwith "Malformed maintainers field"

let get_maintainers acc = function
  | Sexp.(List [Atom "maintainers"; List maintainers]) -> List.map extract_maintainer maintainers @ acc
  | _ -> acc

let fill_pkg_metainfo ~add = function
  | Sexp.(List (List [Atom "name"; Atom name] :: metainfo)) ->
      let maintainers = List.fold_left get_maintainers [] metainfo in
      add name {maintainers}
  | Sexp.List _ -> failwith "Empty package description"
  | Sexp.Atom _ -> failwith "Malformed package description"

let fill_metainfo ~add = function
  | Sexp.(List [List [Atom "version"; Atom "2"]; List [Atom "packages"; List l]]) ->
      List.iter (fill_pkg_metainfo ~add) l
  | Sexp.List _ -> failwith "Version unrecognized"
  | Sexp.Atom _ -> failwith "Malformed file"

let skip_store_error = function
  | Ok x -> Lwt.return x
  | Error err -> Lwt.fail_with (Format.sprintf "Store error %a" Store.pp_error err)

let skip_sync_error = function
  | Ok x -> Lwt.return x
  | Error err -> Lwt.fail_with (Format.sprintf "Sync error %a" Sync.pp_error err)

let fetch_raw_metainfo () =
  Store.v () >>= skip_store_error >>= fun store ->
  let reference = Git.Reference.of_string "refs/heads/index" in
  Sync.clone_ext store ~reference (Uri.of_string "git://github.com/avsm/obi-logs.git") >>= skip_sync_error >>= fun repo ->
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
      | Some (Store.Value.Blob blob) -> Lwt.return (Sexp.of_string (Store.Value.Blob.to_string blob))
      | Some _ -> Lwt.fail_with "Something when wrong with the metadata repository"
      | None -> Lwt.fail_with "Metadata file not found"
      end
  | _ -> assert false
