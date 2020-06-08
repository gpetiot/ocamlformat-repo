val list : unit -> ((string * Fpath.t) list, Rresult.R.msg) Bos.OS.result

val get : string -> Fpath.t option

val install : string -> (unit, Rresult.R.msg) Bos.OS.result
