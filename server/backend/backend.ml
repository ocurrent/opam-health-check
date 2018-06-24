open Lwt.Infix

type t = unit -> Obi.Index.pkg list Lwt.t
type task = unit -> unit Lwt.t

let get_log obi ~comp ~state ~pkg =
  assert false

let get_compilers obi =
  let aux acc x =
    let x = x.Obi.Index.params in
    match x.Obi.Index.arch, x.Obi.Index.distro with
    | `X86_64, `Debian `V9 ->
        let comp = Intf.Compiler.from_string (Ocaml_version.to_string x.Obi.Index.ov) in
        if List.mem ~eq:Intf.Compiler.equal comp acc then
          acc
        else
          comp :: acc
    | _, _ -> acc
  in
  let aux acc (_, x) = List.fold_left aux acc x in
  let aux acc x = List.fold_left aux acc x.Obi.Index.versions in
  List.fold_left aux [] obi

let get_compilers obi =
  obi () >|= fun obi ->
  let compilers = get_compilers obi in
  List.sort Intf.Compiler.compare compilers

let get_pkgs _ obi compilers =
  assert false

let start ~on_finished:_ obi =
  (* TODO: Do a clock that calls on_finished every hour *)
  Lwt.return (obi, fun () -> Lwt.return_unit)
