open Bigarray

let may f x = match x with
  | (Some x) -> f x | None -> ()

let stream = ref None

let buflen = 110
let bufc = Array.make buflen 0.
let buf = [| bufc; |]

let samples_per_sec = 44100.
let beep_duration = 0.04
let two_pi = 4. *. (asin 1.)
let volume = ref 0.375
type state = Tick | Fade | Rest
let state = ref Tick
let duration = ref 0			(* samples *)

let tick_duration = 0.04		(* seconds *)
let fade_samples = 5			(* samples *)
let pitch = ref 607.
let period = ref 109.
let measure_p = ref 0
let new_measure_fn = ref (fun () -> [| (0.,1.) |])
let measure = ref (!new_measure_fn ())

let start fn =
  may Portaudio.start_stream !stream;
  new_measure_fn := fn;
  state := Tick;
  duration := (truncate (samples_per_sec *. tick_duration)) - fade_samples;
  period := samples_per_sec /. !pitch;
  measure := (!new_measure_fn) ();
  measure_p := 0

let bufp = ref 0
let last_val = ref 0.

let update _ out l =
  let set i v = Genarray.set out [|i|] v in
  for i = 0 to pred l do
    (match !state with
      | Tick -> set i (if !period = 0. then 0. else (!volume *. sin (two_pi *. ((mod_float (float_of_int !duration) !period) /. !period))))
      | Rest -> set i 0.
      | Fade -> set i (!last_val *. 0.01)
    );
    last_val := Genarray.get out [|i|];
    decr duration;
    if (!duration) <= 0 then
      (match !state with
	| Tick -> state := Fade; duration := 10
	| Fade -> state := Rest;
	  incr measure_p;
	  if !measure_p >= Array.length !measure then begin
	    measure_p := 0;
	    measure := (!new_measure_fn) ();
	  end;
	  let t = (snd (!measure).(!measure_p)) in
	  duration := truncate ((samples_per_sec *. t) -. tick_duration)
	| Rest -> state := Tick;
	  duration := (truncate (samples_per_sec *. tick_duration)) - fade_samples;
	  let pitch = (fst (!measure).(!measure_p)) in
	  period := if pitch = 0. then 0. else samples_per_sec /. pitch);
  done;
  if !period = 0. then 1 else 0

let init () =
  Portaudio.init ();
  at_exit Portaudio.terminate;
  Printf.printf "Using %s.\n%!" (Portaudio.get_version_string ());
  (* NB: 0 for buffer size == unspecified *)
  stream := Some (Portaudio.open_default_stream ~interleaved:true ~format:Portaudio.format_float32 ~callback:update 0 1 (truncate samples_per_sec) 0)

let stop () =
  may Portaudio.stop_stream !stream

(* 
 * let _ =
 *   init ();
 *   start measure;
 *   (\*update 5.0;*\)
 *   Unix.sleep 5;
 *   stop ();
 *)
