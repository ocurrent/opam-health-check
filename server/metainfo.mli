type t = {
  maintainers : string list;
}

val get_pkgsinfo : unit -> Obi.Index.pkg list Lwt.t
