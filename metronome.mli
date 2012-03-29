
val init : unit -> unit
val start : (unit -> (float*float) array) -> unit
val stop : unit -> unit
val quit : unit -> unit

exception No_device of string
