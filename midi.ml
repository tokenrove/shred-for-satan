
(* 
   - read header, verify magic and count number of tracks
   - read each track, collecting list of relevant events and corresponding bar number
   - merge individual track events into single list of (bar * state changes)
*)

type meter = { numerator:int; denominator:int; }
type key = MajorKey of int | MinorKey of int
type music_state = Tempo of float | Meter of meter | Key of key | EndOfTrack

(* let MS_PER_MINUTE = 6e7 *)

let read_varlen bs =
  bitmatch bs with
    | { false:1; d0:7; rest:-1:bitstring } -> (d0,rest)
    | { true:1; d1:7; false:1; d0:7; rest:-1:bitstring } -> ((d1 lsl 7) lor d0,rest)
    | { true:1; d2:7; true:1; d1:7; false:1; d0:7; rest:-1:bitstring} -> ((d2 lsl 14) lor (d1 lsl 7) lor d0,rest)
    | { true:1; d3:7; true:1; d2:7; true:1; d1:7; false:1; d0:7; rest:-1:bitstring} -> ((d3 lsl 21) lor (d2 lsl 14) lor (d1 lsl 7) lor d0,rest)

let rec read_events accum_t state bs =
  let delta_t, rest = read_varlen bs in
  let accum_t = accum_t + delta_t in
  bitmatch rest with
    | { 0xff:8; 0x2f:8; 0:8 } -> (accum_t,EndOfTrack) :: state
    | { 0xff:8; 0x51:8; 3:8; mspqn:24; rest:-1:bitstring } ->
      (let state = (accum_t, Tempo (6e7/. (float_of_int mspqn))) :: state in
       read_events accum_t state rest)
    | { 0xff:8; 0x58:8; 4:8; numer:8; denom:8:bind(1 lsl denom);
	_:8; _:8; rest:-1:bitstring } ->
      (let state = (accum_t, Meter { numerator=numer; denominator=denom; }) :: state in
       read_events accum_t state rest)
    (* XXX change this back to signed once bitstrings.ml is fixed *)
    | { 0xff:8; 0x59:8; 2:8; accidentals:8:unsigned,bind((accidentals lxor 0x80) - 0x80); scale:8; rest:-1:bitstring } ->
      (let key = (if scale == 0 then MajorKey accidentals else MinorKey accidentals) in
       read_events accum_t ((accum_t, Key key) :: state) rest)
    | { 0xff:8; nature:8; rest:-1:bitstring } ->
      (let len,rest = read_varlen rest in
       bitmatch rest with
	 | { _:len*8:bitstring; rest: -1:bitstring } ->
	   read_events accum_t state rest
	 | { _ } -> raise (Failure "WTF"))
    | { (0xc | 0xd):4; _:4; _:8; rest:-1:bitstring } -> read_events accum_t state rest
      (* type, channel, parameter A, parameter B *)
    | { _:4; _:4; _:8; _:8; rest:-1:bitstring } -> read_events accum_t state rest

let read_track n bs =
  bitmatch bs with
    | { "MTrk": 4*8:string; chunk_len: 32:bind(Int32.mul 8l chunk_len);
	events: (Int32.to_int chunk_len):bitstring; rest: -1:bitstring } ->
      (read_events 0 [] events, rest)

(* Resulting state is in chronological order.
 * Note that this also reverses the list. *)
let merge_key_changes state =
  let fn xs y = match (xs,y) with
    | ((t,Key k) :: rest, (u,Key l)) when (t = u && k = l) -> xs
    | _ -> y :: xs in
  List.fold_left fn [] state

let ticks_per_bar td meter =
  (float_of_int td) *. ((float_of_int meter.numerator) /. ((float_of_int meter.denominator)/.4.))

(* Convert delta_t to bar numbers with time_division
 * Expects state in chronological order. *)
let convert_to_bar_numbers state time_division =
  let rec fn ins outs n t meter =
    let munge u =
      (let dt = u-t in
       n + truncate ((float_of_int dt)/.(ticks_per_bar time_division meter))) in
    match ins with
    | (u,Meter m) :: rest ->
      let n = munge u in fn rest ((n,Meter m) :: outs) n u m
    | (u,ev) :: rest -> let n = munge u in fn rest ((n,ev) :: outs) n u meter
    | [] -> outs
  in
  fn state [] 1 0 {numerator=4; denominator=4;}

let filter_extraneous_end_of_tracks state =
  (List.hd state) :: List.filter (function (_,EndOfTrack) -> false | _ -> true) (List.tl state)

let read_midi_file path =
  let midi_file = Bitstring.bitstring_of_file path in
  bitmatch midi_file with
    | { "MThd": 4*8:string; 6l: 32; format: 16; n_tracks: 16; time_division: 16;
	rest: -1:bitstring } ->
      let rec fn rest state n = if n == 0 then state else
	  let (new_state,rest) = (read_track n rest) in
	  let state = (List.merge (fun a b -> (~-) (compare (fst a) (fst b))) state new_state) in
	  fn rest state (n-1)
      in
      let state = fn rest [] n_tracks in
      let state = filter_extraneous_end_of_tracks state in
      let state = merge_key_changes state in
      let state = convert_to_bar_numbers state time_division in
      state
    | { _ } -> raise (Failure "Not a valid MIDI file")
  ;;

let circle_of_fifths =
  [| ("Ab",415.305);("Eb",311.127);("Bb",466.164);("F",349.228);("C",261.626);
     ("G",391.995);("D",293.665);("A",440.);("E",329.628);("B",493.883);
     ("F#",369.994);("C#",277.183);("G#",415.305);("D#",311.127);("A#",466.144) |]

let remainder x y = match (x mod y) with r when r < 0 -> r+y | r -> r

let pretty_print_key = function
  | MinorKey acc -> (fst circle_of_fifths.(remainder (acc+7) 15)) ^ " minor"
  | MajorKey acc -> (fst circle_of_fifths.(remainder (acc+4) 15)) ^ " major"

let root_pitch_of_key = function
  | MinorKey acc -> (snd circle_of_fifths.(remainder (acc+7) 15))
  | MajorKey acc -> (snd circle_of_fifths.(remainder (acc+4) 15))

let string_of_meter m = (string_of_int m.numerator)^"/"^(string_of_int m.denominator)

(* Stand-alone test function:
 *
 * let _ =
 *   let pp = function | EndOfTrack -> "end of track"
 *     | Meter x -> "meter change: " ^ (string_of_int x.numerator) ^ "/" ^ (string_of_int x.denominator)
 *     | Key k -> "key change: " ^ (pretty_print_key k)
 *     | Tempo x -> "tempo change: " ^ (string_of_float x) in
 *   let state = read_midi_file Sys.argv.(1) in
 *   List.iter (fun (bar,ev) -> Printf.printf "%d: %s\n" bar (pp ev)) state
 *)
