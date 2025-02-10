let compiler_to_json compiler =
  `String (Server_lib.Intf.Compiler.to_string compiler)

let status_to_json state =
  `String (Server_lib.Intf.State.to_string state)

let instance_to_json instance =
  `Assoc [
    ("compiler", compiler_to_json (Server_lib.Intf.Instance.compiler instance));
    ("status", status_to_json (Server_lib.Intf.Instance.state instance));
  ]

let pkg_to_json pkg =
  `Assoc [
    ("name", `String (Server_lib.Intf.Pkg.full_name pkg));
    ("statuses", `List (List.map instance_to_json (Server_lib.Intf.Pkg.instances pkg)));
  ]

let pkgs_to_json pkgs =
  `List (List.map pkg_to_json pkgs)
