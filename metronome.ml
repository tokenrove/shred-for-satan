open Bigarray

exception No_device of string;;

let may f x = match x with | (Some x) -> f x | None -> ()

let stream = ref None

let samples_per_sec = 44100.
let volume = ref 0.8
type state = Tick | Fade | Rest
let state = ref Tick
let duration = ref 0			(* samples *)

let tick_duration = 0.04		(* seconds *)
let fade_samples = 128			(* samples *)
let fade_rate = 0.92
let period = ref 0.
let measure_p = ref 0
let new_measure_fn = ref (fun () -> [| (0.,1.) |])
let measure = ref (!new_measure_fn ())

let inner_vol = ref 0.
type mode = Silence | Sound
let mode = ref Silence

let next_bar () = measure_p := 0; measure := (!new_measure_fn) ()

let next_beat () =
  incr measure_p;
  if !measure_p >= Array.length !measure then next_bar ()

let next_period () =
  let pitch = (fst (!measure).(!measure_p)) in
  if pitch = 0. then 0. else samples_per_sec /. pitch

let tick_duration_samples = (truncate (samples_per_sec *. tick_duration)) - fade_samples

let s_count = ref 0.

let start fn =
  new_measure_fn := fn;
  next_bar ();
  inner_vol := !volume;
  period := next_period ();
  duration := tick_duration_samples;
  s_count := 0.;
  state := Tick;
  mode := Sound

let twopi = 4. *. (asin 1.)
let sine_ct = 256
let sines = let s = Array.make 256 0. in
	    for i = 0 to 255 do
	      s.(i) <- sin (twopi *. ((float_of_int i) /. 255.));
	    done;
	    s
let sine_wave d p =
  let v = ((mod_float d p) /. (p -. 1.)) in
  sines.(min (truncate (256. *. v)) 255)

let update _ out l =
  if !mode == Silence then (Genarray.fill out 0.; 0)
  else
  let set i v = Genarray.set out [|i|] v in
  for i = 0 to pred l do
    let o = (match !state with | Rest -> 0. | _ -> (!inner_vol *. sine_wave !s_count !period)) in
    set i o;
    (match !state with | Fade -> inner_vol := !inner_vol *. fade_rate | _ -> () );
    s_count := !s_count +. 1.;
    decr duration;
    if (!duration) <= 0 then
      (match !state with
	| Tick ->
	  state := Fade; duration := fade_samples;
	| Fade ->
	  state := Rest;
	  let t = (snd (!measure).(!measure_p)) in
	  duration := truncate (samples_per_sec *. (t -. tick_duration))
	| Rest ->
	  state := Tick; inner_vol := !volume; next_beat ();
	  s_count := 0.;
	  period := next_period ();
	  duration := tick_duration_samples);
  done;
  if !period = 0. then mode := Silence;
  flush stdout;
  0

let init () =
  Portaudio.init ();
  at_exit Portaudio.terminate;
  let continue = ref true in
  let i = ref ((Portaudio.get_device_count ())-1) in
  while !continue && !i >= 0 do
    let outparam = Some { Portaudio.channels=1;
			  Portaudio.device=(!i);
			  Portaudio.sample_format=Portaudio.format_float32;
			  Portaudio.latency=0. } in
    (* NB: 0 for buffer size == unspecified *)
    try (stream := Some (Portaudio.open_stream
			   ~interleaved:true
			   ~callback:update
			   None outparam samples_per_sec 0 []);
	 continue := false) with
      | Portaudio.Error e -> decr i;
  done;
  if !continue then
    raise (No_device "Tried our best to get an audio device, but failed.  Make sure jack is running with a 44100 Hz sample rate.");
  may Portaudio.start_stream !stream

let stop () =
  (* may Portaudio.stop_stream !stream *)
  mode := Silence

let quit () =
  may Portaudio.stop_stream !stream

