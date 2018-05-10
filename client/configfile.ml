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

let get_input ~name ~default =
  Printf.printf "%s (default: %s): " name default;
  match read_line () with
  | "" -> default
  | x -> x

let copy_file ~src ~dst =
  IO.with_out ~flags:[Open_creat; Open_excl] dst begin fun out ->
    let content = IO.with_in src IO.read_all in
    output_string out content;
  end

let init_with_values ~confdir ~hostname ~port ~username ~keyfile yamlfile =
  let port = string_of_int port in
  Lwt_main.run (Oca_lib.mkdir_p confdir);
  copy_file ~src:keyfile ~dst:(Filename.concat confdir "default.key");
  IO.with_out ~flags:[Open_creat; Open_excl] yamlfile begin fun out ->
    IO.write_line out "default:";
    IO.write_line out ("  hostname: "^hostname);
    IO.write_line out ("  port: "^port);
    IO.write_line out ("  username: "^username);
  end

let init ~confdir yamlfile =
  let hostname = get_input ~name:"Server hostname" ~default:Oca_lib.localhost in
  let port = get_input ~name:"Server port" ~default:Oca_lib.default_admin_port in
  let username = get_input ~name:"Username" ~default:Oca_lib.default_admin_name in
  let keyfile = get_input ~name:"User key" ~default:"" in
  if String.is_empty keyfile then
    failwith "No key given. Abort.";
  let port = int_of_string port in
  init_with_values ~confdir ~hostname ~port ~username ~keyfile yamlfile

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
  | `O profiles -> List.fold_left parse_profile Map.empty profiles
  | _ -> failwith "Cannot parse the config file"

let profile ~profilename conf =
  let profilename = Option.get_or ~default:"default" profilename in
  Map.find profilename conf

let hostname {hostname; _} = Option.get_exn hostname
let port {port; _} = int_of_string (Option.get_exn port)
let username {username; _} = Option.get_exn username
let keyfile {keyfile; _} = keyfile
