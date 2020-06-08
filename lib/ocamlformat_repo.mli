val list : unit -> ((string * Fpath.t) list, Rresult.R.msg) Bos.OS.result

val get : string -> Fpath.t option

val install : string -> (unit, Rresult.R.msg) Bos.OS.result

val auto :
  enable_outside_detected_project:bool -> root:Fpath.t option -> Fpath.t option
