type t = {
  mutable port : string option;
  mutable admin_port : string option;
}

let create_conf () = {
  port = None;
  admin_port = None;
}

let set_field ~field set = function
  | Some _ -> failwith (Printf.sprintf "Config parser: '%s' is defined twice" field)
  | None -> set ()

let set_config conf = function
  | "port" as field, `String port ->
      set_field ~field (fun () -> conf.port <- Some port) conf.port
  | "admin-port" as field, `String admin_port ->
      set_field ~field (fun () -> conf.admin_port <- Some admin_port) conf.admin_port
  | field, _ ->
      failwith (Printf.sprintf "Config parser: '%s' field not recognized" field)

let yaml_of_conf conf =
  `O [
    "port", `String (Option.get_exn conf.port);
    "admin-port", `String (Option.get_exn conf.admin_port);
  ]

let set_defaults yamlfile conf =
  if Option.is_none conf.port then
    conf.port <- Some Oca_lib.default_html_port;
  if Option.is_none conf.admin_port then
    conf.admin_port <- Some Oca_lib.default_admin_port;
  let yaml = Result.get_exn (Yaml.to_string (yaml_of_conf conf)) in
  IO.with_out (Fpath.to_string yamlfile) (fun out -> output_string out yaml)

let create yamlfile yaml =
  let conf = create_conf () in
  List.iter (set_config conf) yaml;
  set_defaults yamlfile conf;
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
