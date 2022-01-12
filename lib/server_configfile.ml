type t = {
  yamlfile : Fpath.t;
  mutable name : string option;
  mutable port : int option;
  mutable public_url : string option;
  mutable admin_port : int option;
  mutable auto_run_interval : int option;
  mutable processes : int option;
  mutable enable_dune_cache : bool option;
  mutable enable_logs_compression : bool option;
  mutable default_repository : Intf.Github.t option;
  mutable extra_repositories : Intf.Repository.t list option;
  mutable with_test : bool option;
  mutable with_lower_bound : bool option;
  mutable list_command : string option;
  mutable extra_command : string option;
  mutable platform_os : string option;
  mutable platform_arch : string option;
  mutable platform_pool : string option;
  mutable platform_distribution : string option;
  mutable platform_image : string option;
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
  enable_logs_compression = None;
  default_repository = None;
  extra_repositories = None;
  with_test = None;
  with_lower_bound = None;
  list_command = None;
  extra_command = None;
  platform_os = None;
  platform_arch = None;
  platform_pool = None;
  platform_distribution = None;
  platform_image = None;
  ocaml_switches = None;
  slack_webhooks = None;
}

let set_field ~field set = function
  | Some _ -> failwith (Printf.sprintf "Config parser: '%s' is defined twice" field)
  | None -> set ()

let get_comp_str = function
  | `String s -> Intf.Compiler.from_string s
  | _ -> failwith "string expected"

let get_comp = function
  | `O [name, `String switch] -> Intf.Switch.create ~name ~switch
  | _ -> failwith "key and value expected"

let get_repo = function
  | `O [name, `O ["github", `String github]]
  | `O [name, `String github] ->
      Intf.Repository.create ~name ~github ~for_switches:None
  | `O [name, `O [("github", `String github); ("for-switches", `A for_switches)]] ->
      let for_switches = List.map get_comp_str for_switches in
      Intf.Repository.create ~name ~github ~for_switches:(Some for_switches)
  | _ ->
      failwith "key and value expected"

let get_uri = function
  | `String s -> Uri.of_string s
  | _ -> failwith "string expected"

let check_is_docker_compatible name =
  if not (String.for_all Oca_lib.char_is_docker_compatible name) then
    failwith "name field has to contain only alphanumerical characters and '.'"

let set_config conf = function
  | _, `Null -> ()
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
  | "enable-logs-compression" as field, `Bool logs_compression ->
      set_field ~field (fun () -> conf.enable_logs_compression <- Some logs_compression) conf.enable_logs_compression
  | "default-repository" as field, `String github ->
      let repo = Intf.Github.create github in
      set_field ~field (fun () -> conf.default_repository <- Some repo) conf.default_repository
  | "extra-repositories" as field, `A repositories ->
      let repositories = List.map get_repo repositories in
      set_field ~field (fun () -> conf.extra_repositories <- Some repositories) conf.extra_repositories
  | "with-test" as field, `Bool with_test ->
      set_field ~field (fun () -> conf.with_test <- Some with_test) conf.with_test
  | "with-lower-bound" as field, `Bool with_lower_bound ->
      set_field ~field (fun () -> conf.with_lower_bound <- Some with_lower_bound) conf.with_lower_bound
  | "processes" as field, `Float processes ->
      set_field ~field (fun () -> conf.processes <- Some (int_of_float processes)) conf.processes
  | "list-command" as field, `String list_command ->
      set_field ~field (fun () -> conf.list_command <- Some list_command) conf.list_command
  | "extra-command" as field, `String extra_command ->
      set_field ~field (fun () -> conf.extra_command <- Some extra_command) conf.extra_command
  | "platform", `O platform ->
      List.iter (function
        | _, `Null -> ()
        | "os" as field, `String os ->
            set_field ~field (fun () -> conf.platform_os <- Some os) conf.platform_os
        | "arch" as field, `String arch ->
            set_field ~field (fun () -> conf.platform_arch <- Some arch) conf.platform_arch
        | "custom-pool" as field, `String pool ->
            set_field ~field (fun () -> conf.platform_pool <- Some pool) conf.platform_pool
        | "distribution" as field, `String distribution ->
            set_field ~field (fun () -> conf.platform_distribution <- Some distribution) conf.platform_distribution
        | "image" as field, `String image ->
            set_field ~field (fun () -> conf.platform_image <- Some image) conf.platform_image
        | field, _ ->
            failwith (Printf.sprintf "Config parser: '%s' field not recognized" field)
      ) platform
  | "ocaml-switches" as field, `A switches ->
      let switches = List.map get_comp switches in
      set_field ~field (fun () -> conf.ocaml_switches <- Some switches) conf.ocaml_switches
  | "slack-webhooks" as field, `A webhooks ->
      let webhooks = List.map get_uri webhooks in
      set_field ~field (fun () -> conf.slack_webhooks <- Some webhooks) conf.slack_webhooks
  | field, _ ->
      failwith (Printf.sprintf "Config parser: '%s' field not recognized" field)

let yaml_of_extra_repositories l =
  let aux repo =
    match Intf.Repository.for_switches repo with
    | None ->
        `String (Intf.Github.to_string (Intf.Repository.github repo))
    | Some for_switches ->
        `O [
          ("github", `String (Intf.Github.to_string (Intf.Repository.github repo)));
          ("for-switches", `A (List.map (fun s -> `String (Intf.Compiler.to_string s)) for_switches));
        ]
  in
  `A (List.map (fun repo -> `O [Intf.Repository.name repo, aux repo]) l)

let yaml_of_ocaml_switches l =
  `A (List.map (fun s -> `O [Intf.(Compiler.to_string (Switch.name s)), `String (Intf.Switch.switch s)]) l)

let yaml_of_slack_webhooks l =
  `A (List.map (fun s -> `String (Uri.to_string s)) l)

let yaml_of_str_opt = function
  | None -> `Null
  | Some x -> `String x

let yaml_of_conf conf =
  `O [
    "name", `String (Option.get_exn_or "conf.name" conf.name);
    "port", `Float (float_of_int (Option.get_exn_or "conf.port" conf.port));
    "public-url", `String (Option.get_exn_or "conf.public_url" conf.public_url);
    "admin-port", `Float (float_of_int (Option.get_exn_or "conf.admin_port" conf.admin_port));
    "auto-run-interval", `Float (float_of_int (Option.get_exn_or "conf.auto_run_interval" conf.auto_run_interval));
    "processes", `Float (float_of_int (Option.get_exn_or "conf.processes" conf.processes));
    "enable-dune-cache", `Bool (Option.get_exn_or "conf.enable_dune_cache" conf.enable_dune_cache);
    "enable-logs-compression", `Bool (Option.get_exn_or "conf.enable_logs_compression" conf.enable_logs_compression);
    "default-repository", `String (Intf.Github.to_string (Option.get_exn_or "conf.default_repository" conf.default_repository));
    "extra-repositories", Option.map_or ~default:`Null yaml_of_extra_repositories conf.extra_repositories;
    "with-test", `Bool (Option.get_exn_or "conf.with_test" conf.with_test);
    "with-lower-bound", `Bool (Option.get_exn_or "conf.with_lower_bound" conf.with_lower_bound);
    "list-command", `String (Option.get_exn_or "conf.list_command" conf.list_command);
    "extra-command", Option.map_or ~default:`Null (fun s -> `String s) conf.extra_command;
    "platform", `O [
      "os", `String (Option.get_exn_or "conf.platform_os" conf.platform_os);
      "arch", `String (Option.get_exn_or "conf.platform_arch" conf.platform_arch);
      "custom-pool", yaml_of_str_opt conf.platform_pool;
      "distribution", `String (Option.get_exn_or "conf.platform_distribution" conf.platform_distribution);
      "image", `String (Option.get_exn_or "conf.platform_image" conf.platform_image);
    ];
    "ocaml-switches", Option.map_or ~default:`Null yaml_of_ocaml_switches conf.ocaml_switches;
    "slack-webhooks", Option.map_or ~default:`Null yaml_of_slack_webhooks conf.slack_webhooks;
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
  if Option.is_none conf.enable_logs_compression then
    conf.enable_logs_compression <- Some true; (* NOTE: Requires too much disk space for regular users *)
  if Option.is_none conf.default_repository then
    conf.default_repository <- Some (Intf.Github.create "ocaml/opam-repository");
  if Option.is_none conf.extra_repositories then
    conf.extra_repositories <- Some [];
  if Option.is_none conf.with_test then
    conf.with_test <- Some false; (* TODO: Enable by default in the future (takes 1.5x the time) *)
  if Option.is_none conf.with_lower_bound then
    conf.with_lower_bound <- Some false; (* TODO: Enable by default in the future (takes 2x the time) *)
  if Option.is_none conf.list_command then
    conf.list_command <- Some Oca_lib.default_list_command;
  if Option.is_none conf.platform_os then
    conf.platform_os <- Some "linux";
  if Option.is_none conf.platform_arch then
    conf.platform_arch <- Some "x86_64";
  if Option.is_none conf.platform_distribution then
    conf.platform_distribution <- Some "debian-unstable";
  if Option.is_none conf.platform_image then
    conf.platform_image <- Some "ocaml/opam:debian-unstable@sha256:a13c01aab19715953d47831effb2beb0ac90dc98c13b216893db2550799e3b9f";
  if Option.is_none conf.slack_webhooks then
    conf.slack_webhooks <- Some [];
  let yaml = Yaml.to_string_exn (yaml_of_conf conf) in
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
    let%lwt x = f () in
    set_ocaml_switches conf x
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

let set_platform_image conf image =
  conf.platform_image <- Some image;
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

let name {name; _} = Option.get_exn_or "name" name
let port {port; _} = Option.get_exn_or "name" port
let public_url {public_url; _} = Option.get_exn_or "public_url" public_url
let admin_port {admin_port; _} = Option.get_exn_or "admin_port" admin_port
let auto_run_interval {auto_run_interval; _} = Option.get_exn_or "auto_run_interval" auto_run_interval
let processes {processes; _} = Option.get_exn_or "processes" processes
let enable_dune_cache {enable_dune_cache; _} = Option.get_exn_or "enable_dune_cache" enable_dune_cache
let enable_logs_compression {enable_logs_compression; _} = Option.get_exn_or "enable_logs_compression" enable_logs_compression
let default_repository {default_repository; _} = Option.get_exn_or "default_repository" default_repository
let extra_repositories {extra_repositories; _} = Option.get_exn_or "extra_repositories" extra_repositories
let with_test {with_test; _} = Option.get_exn_or "with_test" with_test
let with_lower_bound {with_lower_bound; _} = Option.get_exn_or "with_lower_bound" with_lower_bound
let list_command {list_command; _} = Option.get_exn_or "list_command" list_command
let extra_command {extra_command; _} = extra_command
let platform_os {platform_os; _} = Option.get_exn_or "platform_os" platform_os
let platform_arch {platform_arch; _} = Option.get_exn_or "platform_arch" platform_arch
let platform_pool ({platform_pool; _} as conf) = match platform_pool with
  | None -> platform_os conf^"-"^platform_arch conf
  | Some pool -> pool
let platform_distribution {platform_distribution; _} = Option.get_exn_or "platform_distribution" platform_distribution
let platform_image {platform_image; _} = Option.get_exn_or "platform_image" platform_image
let ocaml_switches {ocaml_switches; _} = ocaml_switches
let slack_webhooks {slack_webhooks; _} = Option.get_exn_or "slack_webhooks" slack_webhooks
