module State = struct
  type t = Good | Partial | Bad | NotAvailable | InternalFailure

  let all = [Good; Partial; Bad; NotAvailable; InternalFailure]

  let equal x y = match x, y with
    | Good, Good | Partial, Partial | Bad, Bad | NotAvailable, NotAvailable | InternalFailure, InternalFailure -> true
    | Good, _ | Partial, _ | Bad, _ | NotAvailable, _ | InternalFailure, _ -> false

  let from_string = function
    | "good" -> Good
    | "partial" -> Partial
    | "bad" -> Bad
    | "not-available" -> NotAvailable
    | "internal-failure" -> InternalFailure
    | _ -> failwith "not a state"

  let to_string = function
    | Good -> "good"
    | Partial -> "partial"
    | Bad -> "bad"
    | NotAvailable -> "not-available"
    | InternalFailure -> "internal-failure"

  let to_pretty_string = function
    | Good -> "Success"
    | Partial -> "Partial failure"
    | Bad -> "Failure"
    | NotAvailable -> "Not available"
    | InternalFailure -> "Internal failure"
end

module Compiler = struct
  type t = Comp of string

  let from_string x =
    if not (Oca_lib.is_valid_filename x) then
      failwith "Forbidden switch name";
    Comp x

  let to_string (Comp x) = x
  let equal (Comp x) (Comp y) = OpamVersionCompare.equal x y
  let compare (Comp x) (Comp y) = OpamVersionCompare.compare x y
end

module Build_with = struct
  type t =
    | Opam
    | Dune

  let compare a b =
    match a, b with
    | Opam, Dune -> -1
    | Dune, Dune
    | Opam, Opam -> 0
    | Dune, Opam -> 1
end


(* TODO: Exchange the name with the Compiler module *)
module Switch = struct
  type t = {
    name: Compiler.t;
    switch: string;
    build_with: Build_with.t;
  }

  let create ~name ~switch ~build_with =
    let name = Compiler.from_string name in
    { name; switch; build_with; }

  let name {name; _} = name
  let switch {switch; _} = switch
  let build_with {build_with; _} = build_with

  let with_dune {build_with; _} =
    match build_with with
    | Build_with.Dune -> true
    | Build_with.Opam -> false

  let equal {name; _} x =
    (* equality of switches is just equality of their names *)
    Compiler.equal name x.name

  let compare {name; build_with; _} x =
    match Compiler.compare name x.name with
    | 0 -> Build_with.compare build_with x.build_with
    | otherwise -> otherwise
end

module Github = struct
  type t = {
    user : string;
    repo : string;
    branch : string option;
  }

  let create github =
    match String.split_on_char '/' github with
    | [user; repo] ->
        begin match String.split_on_char '#' repo with
        | [repo; branch] -> {user; repo; branch = Some branch}
        | [repo] -> {user; repo; branch = None}
        | _ -> failwith "Ill-formed Github repository (expected: user/repo#branch)"
        end
    | _ -> failwith "Ill-formed Github repository (expected: user/repo#branch)"

  let to_string = function
    | {user; repo; branch = None; _} -> user^"/"^repo
    | {user; repo; branch = Some branch; _} -> user^"/"^repo^"#"^branch

  let url {user; repo; _} = "https://github.com/"^user^"/"^repo

  let user {user; _} = user
  let repo {repo; _} = repo
  let branch {branch; _} = branch
end

module Repository = struct
  type t = {
    name : string;
    github : Github.t;
    for_switches : Compiler.t list option;
  }

  let create ~name ~github ~for_switches =
    let github = Github.create github in
    {name; github; for_switches}

  let name {name; _} = name
  let github {github; _} = github
  let for_switches {for_switches; _} = for_switches
end

module Log = struct
  type t = (unit -> string Lwt.t)

  let create f = f

  let to_string f = f ()
end

module Instance = struct
  type t = {
    compiler : Compiler.t;
    state : State.t;
    content : Log.t;
  }

  let create compiler state content = {compiler; state; content}

  let compiler x = x.compiler
  let state x = x.state
  let content x = Log.to_string x.content
end

module Pkg = struct
  type t = {
    full_name : string;
    name : string;
    version : string;
    opam : OpamFile.OPAM.t; (* TODO: Factorize this with the fields above *)
    instances : Instance.t list;
    revdeps : int;
  }

  let create ~full_name ~instances ~opam ~revdeps =
    let (name, version) =
      match String.Split.left ~by:"." full_name with
      | Some x -> x
      | None -> failwith "packages must have a version separated by a dot"
    in
    {full_name; name; version; opam; instances; revdeps}

  let equal x y =
    String.equal x.name y.name &&
    OpamVersionCompare.equal x.version y.version

  let compare x y =
    match String.compare x.name y.name with
    | 0 -> OpamVersionCompare.compare x.version y.version
    | cmp -> cmp

  let full_name x = x.full_name
  let name x = x.name
  let version x = x.version
  let opam x = x.opam
  let instances x = x.instances
  let revdeps x = x.revdeps
end

module Pkg_diff = struct
  type diff =
    | NowInstallable of State.t
    | NotAvailableAnymore of State.t
    | StatusChanged of (State.t * State.t)

  type t = {
    full_name : string;
    comp : Compiler.t;
    diff : diff;
  }
end
