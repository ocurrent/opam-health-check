open Lwt.Infix

type t = {
  yamlfile : Fpath.t;
  mutable port : string option;
  mutable admin_port : string option;
  mutable list_command : string option;
  mutable ocaml_switches : Intf.Compiler.t list option;
  mutable opam_repo_commit_hash : string option;
}

let create_conf yamlfile = {
  yamlfile;
  port = None;
  admin_port = None;
  list_command = None;
  ocaml_switches = None;
  opam_repo_commit_hash = None;
}

let set_field ~field set = function
  | Some _ -> failwith (Printf.sprintf "Config parser: '%s' is defined twice" field)
  | None -> set ()

let get_comp = function
  | `String s -> Intf.Compiler.from_string s
  | _ -> failwith "string expected"

let set_config conf = function
  | "port" as field, `String port ->
      set_field ~field (fun () -> conf.port <- Some port) conf.port
  | "admin-port" as field, `String admin_port ->
      set_field ~field (fun () -> conf.admin_port <- Some admin_port) conf.admin_port
  | "list-command" as field, `String list_command ->
      set_field ~field (fun () -> conf.list_command <- Some list_command) conf.list_command
  | "ocaml-switches", `String "null" -> (* TODO: fix ocaml-yaml's support of null values *)
      ()
  | "ocaml-switches" as field, `A switches ->
      let switches = List.map get_comp switches in
      set_field ~field (fun () -> conf.ocaml_switches <- Some switches) conf.ocaml_switches
  | "opam-repo-commit-hash", `String "null" -> (* TODO: fix ocaml-yaml's support of null values *)
      ()
  | "opam-repo-commit-hash" as field, `String hash ->
      set_field ~field (fun () -> conf.opam_repo_commit_hash <- Some hash) conf.opam_repo_commit_hash
  | field, _ ->
      failwith (Printf.sprintf "Config parser: '%s' field not recognized" field)

let yaml_of_conf conf =
  let null = `String "null" in (* TODO: see above *)
  `O [
    "port", `String (Option.get_exn conf.port);
    "admin-port", `String (Option.get_exn conf.admin_port);
    "list-command", `String (Option.get_exn conf.list_command);
    "ocaml-switches", Option.map_or ~default:null (fun l -> `A (List.map (fun s -> `String (Intf.Compiler.to_string s)) l)) conf.ocaml_switches;
    "opam-repo-commit-hash", Option.map_or ~default:null (fun s -> `String s) conf.opam_repo_commit_hash;
  ]

let set_defaults conf =
  if Option.is_none conf.port then
    conf.port <- Some Oca_lib.default_html_port;
  if Option.is_none conf.admin_port then
    conf.admin_port <- Some Oca_lib.default_admin_port;
  if Option.is_none conf.list_command then
    conf.list_command <- Some Oca_lib.default_list_command;
  let yaml = Result.get_exn (Yaml.to_string (yaml_of_conf conf)) in
  IO.with_out (Fpath.to_string conf.yamlfile) (fun out -> output_string out yaml)

let set_ocaml_switches conf switches =
  conf.ocaml_switches <- Some switches;
  set_defaults conf;
  Lwt.return_unit

let set_default_ocaml_switches conf f =
  if Option.is_none conf.ocaml_switches then
    f () >>= set_ocaml_switches conf
  else
    Lwt.return_unit

let set_list_command conf cmd =
  conf.list_command <- Some cmd;
  set_defaults conf;
  Lwt.return_unit

let set_opam_repo_commit_hash conf hash =
  conf.opam_repo_commit_hash <- Some hash;
  set_defaults conf;
  Lwt.return_unit

let create yamlfile yaml =
  let conf = create_conf yamlfile in
  List.iter (set_config conf) yaml;
  set_defaults conf;
  conf

let from_workdir workdir =
  let yamlfile = Server_workdirs.configfile workdir in
  let yaml = IO.with_in ~flags:[Open_creat] (Fpath.to_string yamlfile) (IO.read_all ?size:None) in
  match Yaml.of_string_exn yaml with
  | `O yaml -> create yamlfile yaml
  | `String "" | `Null -> create yamlfile []
  | _ -> failwith "Config parser: unrecognized config file"

let port {port; _} = int_of_string (Option.get_exn port)
let admin_port {admin_port; _} = int_of_string (Option.get_exn admin_port)
let list_command {list_command; _} = Option.get_exn list_command
let ocaml_switches {ocaml_switches; _} = ocaml_switches
let opam_repo_commit_hash {opam_repo_commit_hash; _} = opam_repo_commit_hash
