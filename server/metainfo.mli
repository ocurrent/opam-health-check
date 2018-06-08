type t = {
  maintainers : string list;
}

val fetch_raw_metainfo : unit -> Sexplib.Sexp.t Lwt.t
val fill_metainfo : add:(string -> t -> unit) -> Sexplib.Sexp.t -> unit
