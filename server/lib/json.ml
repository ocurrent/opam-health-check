let compiler_to_json compiler =
  `String (Intf.Compiler.to_string compiler)

let status_to_json state =
  `String (Intf.State.to_string state)

let instance_to_json instance =
  `Assoc [
    ("compiler", compiler_to_json (Intf.Instance.compiler instance));
    ("status", status_to_json (Intf.Instance.state instance));
  ]

let pkg_to_json pkg =
  `Assoc [
    ("name", `String (Intf.Pkg.full_name pkg));
    ("statuses", `List (List.map instance_to_json (Intf.Pkg.instances pkg)));
  ]

let latest_packages cache =
  match%lwt Cache.get_latest_logdir cache with
  | None ->
      Lwt.return (`List [])
  | Some logdir ->
      let%lwt pkgs = Cache.get_pkgs ~logdir cache in
      let pkgs = List.map pkg_to_json pkgs in
      Lwt.return (`List pkgs)
