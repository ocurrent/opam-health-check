open Lwt.Infix

type check = {
  mutable name : string;
  mutable priority : int option;
  mutable auto_run_interval : int option;
  mutable list_command : string option;
  mutable extra_command : string option;
  mutable ocaml_switches : Intf.Switch.t list option;
  mutable slack_webhooks : Uri.t list option;
}

type t = {
  yamlfile : Fpath.t;
  mutable port : int option;
  mutable admin_port : int option;
  mutable processes : int option;
  mutable checks : check list option;
}

let create_check ~name = {
  name;
  priority = None;
  auto_run_interval = None;
  list_command = None;
  extra_command = None;
  ocaml_switches = None;
  slack_webhooks = None;
}

let create_conf yamlfile = {
  yamlfile;
  port = None;
  admin_port = None;
  processes = None;
  checks = None;
}

let set_field ~field set = function
  | Some _ -> failwith (Printf.sprintf "Config parser: '%s' is defined twice" field)
  | None -> set ()

let get_comp = function
  | `O [name, `String switch] -> Intf.Switch.create ~name ~switch
  | _ -> failwith "key and value expected"

let get_uri = function
  | `String s -> Uri.of_string s
  | _ -> failwith "string expected"

let get_obj = function
  | `O obj -> obj
  | _ -> failwith "object expected"

let check_is_docker_compatible name =
  if not (String.for_all Oca_lib.char_is_docker_compatible name) then
    failwith "name field has to contain only alphanumerical characters and '.'"

let set_check check = function
  | _, `Null ->
      ()
  | "priority" as field, `Float priority ->
      set_field ~field (fun () -> check.priority <- Some (int_of_float priority)) check.priority
  | "auto-run-interval" as field, `Float auto_run_interval ->
      set_field ~field (fun () -> check.auto_run_interval <- Some (int_of_float auto_run_interval)) check.auto_run_interval
  | "list-command" as field, `String list_command ->
      set_field ~field (fun () -> check.list_command <- Some list_command) check.list_command
  | "extra-command" as field, `String extra_command ->
      set_field ~field (fun () -> check.extra_command <- Some extra_command) check.extra_command
  | "ocaml-switches" as field, `A switches ->
      let switches = List.map get_comp switches in
      set_field ~field (fun () -> check.ocaml_switches <- Some switches) check.ocaml_switches
  | "slack-webhooks" as field, `A webhooks ->
      let webhooks = List.map get_uri webhooks in
      set_field ~field (fun () -> check.slack_webhooks <- Some webhooks) check.slack_webhooks
  | field, _ ->
      failwith (Printf.sprintf "Config parser: '%s' field not recognized" field)

let set_config conf = function
  | _, `Null ->
      ()
  | "port" as field, `Float port ->
      set_field ~field (fun () -> conf.port <- Some (int_of_float port)) conf.port
  | "admin-port" as field, `Float admin_port ->
      set_field ~field (fun () -> conf.admin_port <- Some (int_of_float admin_port)) conf.admin_port
  | "processes" as field, `Float processes ->
      set_field ~field (fun () -> conf.processes <- Some (int_of_float processes)) conf.processes
  | "checks" as field, `O checks ->
      let checks = List.map begin fun (name, obj) ->
        check_is_docker_compatible name;
        let check = create_check ~name in
        List.iter (set_check check) (get_obj obj);
        check
      end checks in
      set_field ~field (fun () -> conf.checks <- Some checks) conf.checks
  | field, _ ->
      failwith (Printf.sprintf "Config parser: '%s' field not recognized" field)

let yaml_of_check check =
  let obj = `O [
    "priority", `Float (float_of_int (Option.get_exn check.priority));
    "auto-run-interval", `Float (float_of_int (Option.get_exn check.auto_run_interval));
    "list-command", `String (Option.get_exn check.list_command);
    "extra-command", Option.map_or ~default:`Null (fun s -> `String s) check.extra_command;
    "ocaml-switches", Option.map_or ~default:`Null (fun l -> `A (List.map (fun s -> `O [Intf.(Compiler.to_string (Switch.name s)), `String (Intf.Switch.switch s)]) l)) check.ocaml_switches;
    "slack-webhooks", Option.map_or ~default:`Null (fun l -> `A (List.map (fun s -> `String (Uri.to_string s)) l)) check.slack_webhooks;
  ] in
  (check.name, obj)

let yaml_of_conf conf =
  `O [
    "port", `Float (float_of_int (Option.get_exn conf.port));
    "admin-port", `Float (float_of_int (Option.get_exn conf.admin_port));
    "processes", `Float (float_of_int (Option.get_exn conf.processes));
    "checks", `O (List.map yaml_of_check (Option.get_exn conf.checks));
  ]

let set_check_defaults check =
  if Option.is_none check.priority then
    check.priority <- Some 100;
  if Option.is_none check.auto_run_interval then
    check.auto_run_interval <- Some Oca_lib.default_auto_run_interval;
  if Option.is_none check.list_command then
    check.list_command <- Some Oca_lib.default_list_command;
  if Option.is_none check.slack_webhooks then
    check.slack_webhooks <- Some [];
  check

let set_defaults conf =
  if Option.is_none conf.port then
    conf.port <- Some (int_of_string Oca_lib.default_html_port);
  if Option.is_none conf.admin_port then
    conf.admin_port <- Some (int_of_string Oca_lib.default_admin_port);
  if Option.is_none conf.processes then
    conf.processes <- Some Oca_lib.default_processes;
  if Option.is_none conf.checks then
    conf.checks <- Some [set_check_defaults (create_check ~name:Oca_lib.default_server_name)];
  let yaml = Result.get_exn (Yaml.to_string (yaml_of_conf conf)) in
  IO.with_out (Fpath.to_string conf.yamlfile) (fun out -> output_string out yaml)

let set_processes conf i =
  conf.processes <- Some i;
  set_defaults conf;
  Lwt.return_unit

let set_priority conf check i =
  check.priority <- Some i;
  set_defaults conf;
  Lwt.return_unit

let set_auto_run_interval conf check i =
  check.auto_run_interval <- Some i;
  set_defaults conf;
  Lwt.return_unit

let set_ocaml_switches conf check switches =
  check.ocaml_switches <- Some switches;
  set_defaults conf;
  Lwt.return_unit

let set_default_ocaml_switches conf check f =
  if Option.is_none check.ocaml_switches then
    f () >>= set_ocaml_switches conf check
  else
    Lwt.return_unit

let set_list_command conf check cmd =
  check.list_command <- Some cmd;
  set_defaults conf;
  Lwt.return_unit

let set_extra_command conf check cmd =
  check.extra_command <- cmd;
  set_defaults conf;
  Lwt.return_unit

let set_slack_webhooks conf check webhooks =
  check.slack_webhooks <- Some webhooks;
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

let port {port; _} = Option.get_exn port
let admin_port {admin_port; _} = Option.get_exn admin_port
let processes {processes; _} = Option.get_exn processes
let checks {checks; _} = Option.get_exn checks

let name {name; _} = name
let priority {priority; _} = Option.get_exn priority
let auto_run_interval {auto_run_interval; _} = Option.get_exn auto_run_interval
let list_command {list_command; _} = Option.get_exn list_command
let extra_command {extra_command; _} = extra_command
let ocaml_switches {ocaml_switches; _} = ocaml_switches
let slack_webhooks {slack_webhooks; _} = Option.get_exn slack_webhooks

let get_check conf ~check_name =
  List.find (fun x -> String.equal check_name x.name) (Option.get_exn conf.checks)
