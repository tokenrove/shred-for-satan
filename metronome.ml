open Bigarray

let may f x = match x with | (Some x) -> f x | None -> ()

let stream = ref None

let samples_per_sec = 44100.
let two_pi = 4. *. (asin 1.)
let volume = ref 0.375
type state = Tick | Fade | Rest
let state = ref Tick
let duration = ref 0			(* samples *)

let tick_duration = 0.04		(* seconds *)
let fade_samples = 10			(* samples *)
let period = ref 109.
let measure_p = ref 0
let new_measure_fn = ref (fun () -> [| (0.,1.) |])
let measure = ref (!new_measure_fn ())

let inner_vol = ref 0.

let start fn =
  new_measure_fn := fn;
  measure := (!new_measure_fn) ();
  measure_p := 0;
  state := Tick;
  inner_vol := !volume;
  duration := (truncate (samples_per_sec *. tick_duration)) - fade_samples;
  let pitch = (fst (!measure).(!measure_p)) in
  period := if pitch = 0. then 0. else samples_per_sec /. pitch;
  may Portaudio.start_stream !stream

let sine_wave () = sin (two_pi *. ((mod_float (float_of_int !duration) !period) /. (!period -. 1.)))

let next_bar () =
  incr measure_p;
  if !measure_p >= Array.length !measure then begin
    measure_p := 0;
    measure := (!new_measure_fn) ();
  end


let update _ out l =
  let set i v = Genarray.set out [|i|] v in
  for i = 0 to pred l do
    set i (if !period = 0. then 0. else (!inner_vol *. sine_wave ()));
    (match !state with | Fade -> inner_vol := !inner_vol *. 0.01 | _ -> () );
    decr duration;
    if (!duration) <= 0 then
      (match !state with
	| Tick -> state := Fade; duration := fade_samples;
	| Fade -> state := Rest; inner_vol := 0.;
	  let t = (snd (!measure).(!measure_p)) in
	  duration := truncate ((samples_per_sec *. t) -. tick_duration)
	| Rest -> state := Tick; inner_vol := !volume; next_bar ();
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
