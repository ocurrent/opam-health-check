open Containers

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

let set_defaults yamlfile conf =
  let r = ref [] in
  if Option.is_none conf.port then begin
    r := ("port: "^Oca_lib.default_html_port) :: !r;
    conf.port <- Some Oca_lib.default_html_port;
  end;
  if Option.is_none conf.admin_port then begin
    r := ("admin-port: "^Oca_lib.default_admin_port) :: !r;
    conf.admin_port <- Some Oca_lib.default_admin_port;
  end;
  if not (List.is_empty !r) then begin
    IO.with_out_a yamlfile begin fun out ->
      (* NOTE: Always prepend a newline in case the file doesn't end with one *)
      IO.write_line out "";
      List.iter (IO.write_line out) !r;
    end;
  end

let create yamlfile yaml =
  let conf = create_conf () in
  List.iter (set_config conf) yaml;
  set_defaults yamlfile conf;
  conf

let from_workdir workdir =
  let yamlfile = Server_workdirs.configfile workdir in
  let yaml = IO.with_in ~flags:[Open_creat] yamlfile IO.read_all in
  match Yaml.of_string_exn yaml with
  | `O yaml -> create yamlfile yaml
  | `String "" | `Null -> create yamlfile []
  | _ -> failwith "Config parser: unrecognized config file"

let port {port; _} = int_of_string (Option.get_exn port)
let admin_port {admin_port; _} = int_of_string (Option.get_exn admin_port)
