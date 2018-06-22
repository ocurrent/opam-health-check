module State = struct
  type t = Good | Partial | Bad

  let equal x y = match x, y with
    | Good, Good | Partial, Partial | Bad, Bad -> true
    | Good, _ | Partial, _ | Bad, _ -> false
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

module Instance = struct
  type t = {
    compiler : Compiler.t;
    state : State.t;
  }

  let create compiler state = {compiler; state}

  let compiler x = x.compiler
  let state x = x.state
end

module Pkg = struct
  type t = {
    full_name : string;
    name : string;
    maintainers : string list;
    instances : Instance.t list;
  }

  let create ~full_name ~instances ~maintainers =
    let name =
      match String.index_opt full_name '.' with
      | Some idx -> String.sub full_name 0 idx
      | None -> failwith "packages must have a version separated by a dot"
    in
    {full_name; name; maintainers; instances}

  let equal x y = OpamVersionCompare.equal x.full_name y.full_name
  let compare x y = OpamVersionCompare.compare x.full_name y.full_name

  let full_name x = x.full_name
  let name x = x.name
  let maintainers x = x.maintainers
  let instances x = x.instances
end
