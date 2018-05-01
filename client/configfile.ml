open Containers

type profile = {
  keyfile : string;
  mutable hostname : string option;
  mutable port : string option;
  mutable username : string option;
}

module Map = Map.Make (String)

type t = profile Map.t

let empty_profile profilename = {
  keyfile = profilename^".key";
  hostname = None;
  port = None;
  username = None;
}

let create_example yamlfile =
  IO.with_out_a yamlfile begin fun out ->
    (* NOTE: Always prepend a newline in case the file doesn't end with one *)
    IO.write_line out "";
    IO.write_line out "default:";
    IO.write_line out ("  - hostname: localhost");
    IO.write_line out ("  - port: "^Oca_lib.default_admin_port);
    IO.write_line out ("  - username: "^Oca_lib.default_admin_name);
  end

let set_field ~field set = function
  | Some _ -> failwith (Printf.sprintf "Config parser: '%s' is defined twice" field)
  | None -> set ()

let parse_profile_fields p = function
  | "hostname" as field, `String hostname ->
      set_field ~field (fun () -> p.hostname <- Some hostname) p.hostname
  | "port" as field, `String port ->
      set_field ~field (fun () -> p.port <- Some port) p.port
  | "username" as field, `String username ->
      set_field ~field (fun () -> p.username <- Some username) p.username
  | field, _ ->
      failwith (Printf.sprintf "Config parser: '%s' field not recognized" field)

let check_missing_fields {hostname; port; username} =
  if Option.is_none hostname then begin
    failwith "Config parser: Missing 'hostname' field";
  end;
  if Option.is_none port then begin
    failwith "Config parser: Missing 'port' field";
  end;
  if Option.is_none username then begin
    failwith "Config parser: Missing 'username' field";
  end

let parse_profile profiles = function
  | profile, _ when Map.mem profile profiles ->
      failwith "Profile name already defined"
  | profile, `O fields ->
      let p = empty_profile profile in
      List.iter (parse_profile_fields p) fields;
      check_missing_fields p;
      Map.add profile p profiles
  | _, _ ->
      failwith "Cannot parse"

let from_file yamlfile =
  let yaml = IO.with_in ~flags:[Open_creat] yamlfile IO.read_all in
  match Yaml.of_string_exn yaml with
  | `String "" | `Null -> create_example yamlfile; None
  | `O profiles -> Some (List.fold_left parse_profile Map.empty profiles)
  | _ -> failwith "Cannot parse the config file"

let profile ~profilename conf =
  let profilename = Option.get_or ~default:"default" profilename in
  Map.find profilename conf

let hostname {hostname; _} = Option.get_exn hostname
let port {port; _} = int_of_string (Option.get_exn port)
let username {username; _} = Option.get_exn username
let keyfile {keyfile; _} = keyfile
