open Lwt.Infix

type t = unit -> Obi.Index.pkg list Lwt.t
type task = unit -> unit Lwt.t

(* TODO: Do a PR to ocaml-dockerfile *)
let arch_eq x y = match x, y with
  | `X86_64, `X86_64
  | `Aarch64, `Aarch64
  | `Ppc64le, `Ppc64le -> true
  | (`X86_64 | `Aarch64 | `Ppc64le), _ -> false

let params_eq {Obi.Index.arch; distro; ov} comp =
  String.equal (Ocaml_version.to_string ov) (Intf.Compiler.to_string comp) &&
  Dockerfile_distro.compare distro (`Debian `V9) = 0 &&
  arch_eq arch `X86_64

let get_log obi ~comp ~state ~pkg =
  obi () >|= fun obi ->
  let pkg = Intf.Pkg.create ~full_name:pkg ~instances:[] ~maintainers:[] in
  let pkg_name = Intf.Pkg.name pkg in
  match List.find_opt (fun p -> String.equal p.Obi.Index.name pkg_name) obi with
  | Some {Obi.Index.versions; _} ->
      let pkg_ver = Intf.Pkg.version pkg in
      begin match List.assoc_opt ~eq:String.equal pkg_ver versions with
      | Some metadata ->
          begin match List.find_opt (fun {Obi.Index.params; _} -> params_eq params comp) metadata, state with
          | Some {Obi.Index.build_result = `Ok as build_result; log; _}, Intf.State.Good
          | Some {Obi.Index.build_result = `Fail _ as build_result; log; _}, Intf.State.Bad
          | Some {Obi.Index.build_result = _ as build_result; log; _}, Intf.State.Partial ->
              Format.sprintf "%a\n\n%s"
                Obi.Index.pp_result build_result
                (String.concat "\n" log)
          | Some _, _ -> failwith "Can't find the log with the good state"
          | None, _ -> failwith "Can't find the right compiler version / env"
          end
      | None -> failwith "Can't find the right package version"
      end
  | None ->
      failwith "Can't find the package name"

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

let get_pkgs obi compilers =
  List.fold_left
    begin fun acc {Obi.Index.name; maintainers; versions; _} ->
      List.fold_left
        begin fun acc (v, metadata) ->
          let instances =
            List.fold_left
              begin fun acc {Obi.Index.params; build_result; _} ->
                if List.exists (params_eq params) compilers then
                  let comp = Intf.Compiler.from_string (Ocaml_version.to_string params.Obi.Index.ov) in
                  match build_result with
                  | `Ok -> Intf.Instance.create comp Intf.State.Good :: acc
                  | `Fail _ -> Intf.Instance.create comp Intf.State.Bad :: acc
                  | `Uninstallable _ -> acc
                  | _ -> Intf.Instance.create comp Intf.State.Partial :: acc
                else
                  acc
              end
              []
              metadata
          in
          Intf.Pkg.create ~full_name:(name^"."^v) ~instances ~maintainers :: acc
        end
        acc
        versions
    end
    []
    obi

let get_pkgs _ obi compilers =
  obi >>= fun obi ->
  compilers >|= fun compilers ->
  let pkgs = get_pkgs obi compilers in
  List.sort Intf.Pkg.compare pkgs

let start ~on_finished:_ obi =
  (* TODO: Do a clock that calls on_finished every hour *)
  Lwt.return (obi, fun () -> Lwt.return_unit)
