open Lwt.Infix

module State = struct
  type t = Good | Partial | Bad

  let equal x y = match x, y with
    | Good, Good | Partial, Partial | Bad, Bad -> true
    | Good, _ | Partial, _ | Bad, _ -> false

  let from_string = function
    | "good" -> Good
    | "partial" -> Partial
    | "bad" -> Bad
    | _ -> failwith "not a state"

  let to_string = function
    | Good -> "good"
    | Partial -> "partial"
    | Bad -> "bad"
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

module Log = struct
  type t =
    | Compressed of bytes Lwt.t
    | Unstored of (unit -> string Lwt.t)

  let compressed_buffer_len = ref 0

  let compressed s =
    let s =
      s >|= fun s ->
      compressed_buffer_len := max !compressed_buffer_len (String.length s);
      LZ4.Bytes.compress (Bytes.unsafe_of_string s)
    in
    Compressed s
  let unstored f = Unstored f

  let to_string = function
    | Compressed s -> s >|= fun s -> Bytes.unsafe_to_string (LZ4.Bytes.decompress ~length:!compressed_buffer_len s)
    | Unstored f -> f ()
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
    maintainers : string list;
    instances : Instance.t list;
  }

  let create ~full_name ~instances ~maintainers =
    let (name, version) =
      match String.Split.left ~by:"." full_name with
      | Some x -> x
      | None -> failwith "packages must have a version separated by a dot"
    in
    {full_name; name; version; maintainers; instances}

  let equal x y = OpamVersionCompare.equal x.full_name y.full_name
  let compare x y = OpamVersionCompare.compare x.full_name y.full_name

  let full_name x = x.full_name
  let name x = x.name
  let version x = x.version
  let maintainers x = x.maintainers
  let instances x = x.instances
end

module Pkg_diff = struct
  type diff =
    | NowInstallable
    | NotAvailableAnymore
    | StatusChanged of (State.t * State.t)

  type t = {
    full_name : string;
    comp : Compiler.t;
    diff : diff;
  }
end
