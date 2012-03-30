
type meter = { numerator:int; denominator:int; }
type key = MajorKey of int | MinorKey of int
type music_state = Tempo of float | Meter of meter | Key of key | EndOfTrack

val read_midi_file : string -> (int * music_state) list
val pretty_print_key : key -> string
val root_pitch_of_key : key -> float
val string_of_meter : meter -> string
