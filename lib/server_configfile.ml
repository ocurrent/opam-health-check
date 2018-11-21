type t = {
  yamlfile : Fpath.t;
  mutable port : string option;
  mutable admin_port : string option;
  mutable list_command : string option;
  mutable ocaml_switches : Intf.Compiler.t list option;
}

let create_conf yamlfile = {
  yamlfile;
  port = None;
  admin_port = None;
  list_command = None;
  ocaml_switches = None;
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
  | "ocaml-switches" as field, `A switches ->
      let switches = List.map get_comp switches in
      set_field ~field (fun () -> conf.ocaml_switches <- Some switches) conf.ocaml_switches
  | field, _ ->
      failwith (Printf.sprintf "Config parser: '%s' field not recognized" field)

let yaml_of_conf conf =
  `O [
    "port", `String (Option.get_exn conf.port);
    "admin-port", `String (Option.get_exn conf.admin_port);
    "list-command", `String (Option.get_exn conf.list_command);
    "ocaml-switches", `A (List.map (fun s -> `String (Intf.Compiler.to_string s)) (Option.get_exn conf.ocaml_switches))
  ]

let set_defaults conf =
  if Option.is_none conf.port then
    conf.port <- Some Oca_lib.default_html_port;
  if Option.is_none conf.admin_port then
    conf.admin_port <- Some Oca_lib.default_admin_port;
  if Option.is_none conf.list_command then
    conf.list_command <- Some Oca_lib.default_list_command;
  if Option.is_none conf.ocaml_switches then
    conf.ocaml_switches <- Some Oca_lib.default_ocaml_switches;
  let yaml = Result.get_exn (Yaml.to_string (yaml_of_conf conf)) in
  IO.with_out (Fpath.to_string conf.yamlfile) (fun out -> output_string out yaml)

let set_ocaml_switches conf switches =
  conf.ocaml_switches <- Some switches;
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
let ocaml_switches {ocaml_switches; _} = Option.get_exn ocaml_switches
