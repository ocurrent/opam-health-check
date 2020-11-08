open Lwt.Infix

type t = {
  yamlfile : Fpath.t;
  mutable name : string option;
  mutable port : int option;
  mutable public_url : string option;
  mutable admin_port : int option;
  mutable auto_run_interval : int option;
  mutable processes : int option;
  mutable enable_dune_cache : bool option;
  mutable enable_in_memory_logs : bool option;
  mutable extra_repositories : Intf.Repository.t list option;
  mutable with_test : bool option;
  mutable list_command : string option;
  mutable extra_command : string option;
  mutable ocaml_switches : Intf.Switch.t list option;
  mutable slack_webhooks : Uri.t list option;
}

let create_conf yamlfile = {
  yamlfile;
  name = None;
  port = None;
  public_url = None;
  admin_port = None;
  auto_run_interval = None;
  processes = None;
  enable_dune_cache = None;
  enable_in_memory_logs = None;
  extra_repositories = None;
  with_test = None;
  list_command = None;
  extra_command = None;
  ocaml_switches = None;
  slack_webhooks = None;
}

let set_field ~field set = function
  | Some _ -> failwith (Printf.sprintf "Config parser: '%s' is defined twice" field)
  | None -> set ()

let get_comp = function
  | `O [name, `String switch] -> Intf.Switch.create ~name ~switch
  | _ -> failwith "key and value expected"

let get_repo = function
  | `O [name, `String github] -> Intf.Repository.create ~name ~github
  | _ -> failwith "key and value expected"

let get_uri = function
  | `String s -> Uri.of_string s
  | _ -> failwith "string expected"

let check_is_docker_compatible name =
  if not (String.for_all Oca_lib.char_is_docker_compatible name) then
    failwith "name field has to contain only alphanumerical characters and '.'"

let set_config conf = function
  | _, `Null ->
      ()
  | "name" as field, `String name ->
      check_is_docker_compatible name;
      set_field ~field (fun () -> conf.name <- Some name) conf.name
  | "port" as field, `Float port ->
      set_field ~field (fun () -> conf.port <- Some (int_of_float port)) conf.port
  | "public-url" as field, `String public_url ->
      set_field ~field (fun () -> conf.public_url <- Some public_url) conf.public_url
  | "admin-port" as field, `Float admin_port ->
      set_field ~field (fun () -> conf.admin_port <- Some (int_of_float admin_port)) conf.admin_port
  | "auto-run-interval" as field, `Float auto_run_interval ->
      set_field ~field (fun () -> conf.auto_run_interval <- Some (int_of_float auto_run_interval)) conf.auto_run_interval
  | "enable-dune-cache" as field, `Bool dune_cache ->
      set_field ~field (fun () -> conf.enable_dune_cache <- Some dune_cache) conf.enable_dune_cache
  | "enable-in-memory-logs" as field, `Bool in_memory_logs ->
      set_field ~field (fun () -> conf.enable_in_memory_logs <- Some in_memory_logs) conf.enable_in_memory_logs
  | "extra-repositories" as field, `A repositories ->
      let repositories = List.map get_repo repositories in
      set_field ~field (fun () -> conf.extra_repositories <- Some repositories) conf.extra_repositories
  | "with-test" as field, `Bool with_test ->
      set_field ~field (fun () -> conf.with_test <- Some with_test) conf.with_test
  | "processes" as field, `Float processes ->
      set_field ~field (fun () -> conf.processes <- Some (int_of_float processes)) conf.processes
  | "list-command" as field, `String list_command ->
      set_field ~field (fun () -> conf.list_command <- Some list_command) conf.list_command
  | "extra-command" as field, `String extra_command ->
      set_field ~field (fun () -> conf.extra_command <- Some extra_command) conf.extra_command
  | "ocaml-switches" as field, `A switches ->
      let switches = List.map get_comp switches in
      set_field ~field (fun () -> conf.ocaml_switches <- Some switches) conf.ocaml_switches
  | "slack-webhooks" as field, `A webhooks ->
      let webhooks = List.map get_uri webhooks in
      set_field ~field (fun () -> conf.slack_webhooks <- Some webhooks) conf.slack_webhooks
  | field, _ ->
      failwith (Printf.sprintf "Config parser: '%s' field not recognized" field)

let yaml_of_conf conf =
  `O [
    "name", `String (Option.get_exn conf.name);
    "port", `Float (float_of_int (Option.get_exn conf.port));
    "public-url", `String (Option.get_exn conf.public_url);
    "admin-port", `Float (float_of_int (Option.get_exn conf.admin_port));
    "auto-run-interval", `Float (float_of_int (Option.get_exn conf.auto_run_interval));
    "processes", `Float (float_of_int (Option.get_exn conf.processes));
    "enable-dune-cache", `Bool (Option.get_exn conf.enable_dune_cache);
    "enable-in-memory-logs", `Bool (Option.get_exn conf.enable_in_memory_logs);
    "extra-repositories", Option.map_or ~default:`Null (fun l -> `A (List.map (fun s -> `O [Intf.Repository.name s, `String (Intf.Repository.github s)]) l)) conf.extra_repositories;
    "with-test", `Bool (Option.get_exn conf.with_test);
    "list-command", `String (Option.get_exn conf.list_command);
    "extra-command", Option.map_or ~default:`Null (fun s -> `String s) conf.extra_command;
    "ocaml-switches", Option.map_or ~default:`Null (fun l -> `A (List.map (fun s -> `O [Intf.(Compiler.to_string (Switch.name s)), `String (Intf.Switch.switch s)]) l)) conf.ocaml_switches;
    "slack-webhooks", Option.map_or ~default:`Null (fun l -> `A (List.map (fun s -> `String (Uri.to_string s)) l)) conf.slack_webhooks;
  ]

let set_defaults conf =
  if Option.is_none conf.name then
    conf.name <- Some Oca_lib.default_server_name;
  if Option.is_none conf.port then
    conf.port <- Some (int_of_string Oca_lib.default_html_port);
  if Option.is_none conf.public_url then
    conf.public_url <- Some Oca_lib.default_public_url;
  if Option.is_none conf.admin_port then
    conf.admin_port <- Some (int_of_string Oca_lib.default_admin_port);
  if Option.is_none conf.auto_run_interval then
    conf.auto_run_interval <- Some Oca_lib.default_auto_run_interval;
  if Option.is_none conf.processes then
    conf.processes <- Some Oca_lib.default_processes;
  if Option.is_none conf.enable_dune_cache then
    conf.enable_dune_cache <- Some false; (* NOTE: Too unstable to enable by default *)
  if Option.is_none conf.enable_in_memory_logs then
    conf.enable_in_memory_logs <- Some false; (* NOTE: Requires too much memory for regular users *)
  if Option.is_none conf.extra_repositories then
    conf.extra_repositories <- Some [Intf.Repository.create ~name:"beta" ~github:"ocaml/ocaml-beta-repository"];
  if Option.is_none conf.with_test then
    conf.with_test <- Some false; (* TODO: Enable by default in the future (takes 1.5x the time) *)
  if Option.is_none conf.list_command then
    conf.list_command <- Some Oca_lib.default_list_command;
  if Option.is_none conf.slack_webhooks then
    conf.slack_webhooks <- Some [];
  let yaml = Result.get_exn (Yaml.to_string (yaml_of_conf conf)) in
  IO.with_out (Fpath.to_string conf.yamlfile) (fun out -> output_string out yaml)

let set_auto_run_interval conf i =
  conf.auto_run_interval <- Some i;
  set_defaults conf;
  Lwt.return_unit

let set_processes conf i =
  conf.processes <- Some i;
  set_defaults conf;
  Lwt.return_unit

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

let set_extra_command conf cmd =
  conf.extra_command <- cmd;
  set_defaults conf;
  Lwt.return_unit

let set_slack_webhooks conf webhooks =
  conf.slack_webhooks <- Some webhooks;
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

let name {name; _} = Option.get_exn name
let port {port; _} = Option.get_exn port
let public_url {public_url; _} = Option.get_exn public_url
let admin_port {admin_port; _} = Option.get_exn admin_port
let auto_run_interval {auto_run_interval; _} = Option.get_exn auto_run_interval
let processes {processes; _} = Option.get_exn processes
let enable_dune_cache {enable_dune_cache; _} = Option.get_exn enable_dune_cache
let enable_in_memory_logs {enable_in_memory_logs; _} = Option.get_exn enable_in_memory_logs
let extra_repositories {extra_repositories; _} = Option.get_exn extra_repositories
let with_test {with_test; _} = Option.get_exn with_test
let list_command {list_command; _} = Option.get_exn list_command
let extra_command {extra_command; _} = extra_command
let ocaml_switches {ocaml_switches; _} = ocaml_switches
let slack_webhooks {slack_webhooks; _} = Option.get_exn slack_webhooks
