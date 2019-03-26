type profile = {
  keyfile : Fpath.t;
  mutable hostname : string option;
  mutable port : int option;
  mutable username : string option;
}

module Map = Map.Make (String)

type t = profile Map.t

let empty_profile ~confdir profilename = {
  keyfile = Fpath.add_ext "key" (Fpath.add_seg confdir profilename);
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
  IO.with_out ~flags:[Open_creat; Open_excl] (Fpath.to_string dst) begin fun out ->
    let content = IO.with_in (Fpath.to_string src) (IO.read_all ?size:None) in
    output_string out content;
  end

let yaml_of_conf ~profilename ~hostname ~port ~username =
  "\n"^
  profilename^":\n"^
  "  hostname: "^hostname^"\n"^
  "  port: "^port^"\n"^
  "  username: "^username^"\n"

let init_with_values ~confdir ~profilename ~hostname ~port ~username ~keyfile yamlfile =
  let port = string_of_int port in
  Lwt_main.run (Oca_lib.mkdir_p confdir);
  copy_file ~src:keyfile ~dst:(Fpath.add_seg confdir (profilename^".key"));
  let new_profile_str = yaml_of_conf ~profilename ~hostname ~port ~username in
  IO.with_out_a (Fpath.to_string yamlfile) (fun out -> output_string out new_profile_str)

let init ~confdir yamlfile =
  let profilename = get_input ~name:"Profile name" ~default:Oca_lib.default_server_name in
  let hostname = get_input ~name:"Server hostname" ~default:Oca_lib.localhost in
  let port = get_input ~name:"Server port" ~default:Oca_lib.default_admin_port in
  let username = get_input ~name:"Username" ~default:Oca_lib.default_admin_name in
  let keyfile = get_input ~name:"User key" ~default:"" in
  let keyfile = Fpath.v keyfile in
  let port = int_of_string port in
  init_with_values ~confdir ~profilename ~hostname ~port ~username ~keyfile yamlfile

let set_field ~field set = function
  | Some _ -> failwith (Printf.sprintf "Config parser: '%s' is defined twice" field)
  | None -> set ()

let parse_profile_fields p = function
  | "hostname" as field, `String hostname ->
      set_field ~field (fun () -> p.hostname <- Some hostname) p.hostname
  | "port" as field, `Float port ->
      set_field ~field (fun () -> p.port <- Some (int_of_float port)) p.port
  | "username" as field, `String username ->
      set_field ~field (fun () -> p.username <- Some username) p.username
  | field, _ ->
      failwith (Printf.sprintf "Config parser: '%s' field not recognized" field)

let check_missing_fields {keyfile = _; hostname; port; username} =
  if Option.is_none hostname then begin
    failwith "Config parser: Missing 'hostname' field";
  end;
  if Option.is_none port then begin
    failwith "Config parser: Missing 'port' field";
  end;
  if Option.is_none username then begin
    failwith "Config parser: Missing 'username' field";
  end

let parse_profile ~confdir profiles = function
  | profile, _ when Map.mem profile profiles ->
      failwith "Profile name already defined"
  | profile, `O fields ->
      if not (Oca_lib.is_valid_filename profile) then
        failwith "Profile name containers forbidden characters";
      let p = empty_profile ~confdir profile in
      List.iter (parse_profile_fields p) fields;
      check_missing_fields p;
      Map.add profile p profiles
  | _, _ ->
      failwith "Cannot parse"

let from_file ~confdir yamlfile =
  let yaml = IO.with_in ~flags:[Open_creat] (Fpath.to_string yamlfile) (IO.read_all ?size:None) in
  match Yaml.of_string_exn yaml with
  | `O profiles -> List.fold_left (parse_profile ~confdir) Map.empty profiles
  | _ -> failwith "Cannot parse the config file"

let profile ~profilename conf =
  Map.find profilename conf

let hostname {hostname; _} = Option.get_exn hostname
let port {port; _} = Option.get_exn port
let username {username; _} = Option.get_exn username
let keyfile {keyfile; _} = keyfile
