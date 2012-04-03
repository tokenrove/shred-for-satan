(* Copyright 2011 Julian Squires <julian@cipht.net>
 * Please see the COPYING file for license information. *)

open Bigarray

exception No_device of string;;

let may f x = match x with | (Some x) -> f x | None -> ()

let tick_duration = 0.04		(* seconds *)
let fade_samples = 128			(* samples *)
let fade_rate = 0.92

let stream = ref None
let samples_per_sec = ref 44100.
let volume = ref 0.8

let period = ref 0.
let measure_p = ref 0
let new_measure_fn = ref (fun () -> [| (0.,1.) |])
let measure = ref (!new_measure_fn ())

let inner_vol = ref 0.
type state = Tick | Fade | Rest | Stopped
let state = ref Stopped
let duration = ref 0			(* samples *)

let next_bar () = measure_p := 0; measure := (!new_measure_fn) ()

let next_beat () =
  incr measure_p;
  if !measure_p >= Array.length !measure then next_bar ()

let next_period () =
  let pitch = (fst (!measure).(!measure_p)) in
  if pitch = 0. then 0. else !samples_per_sec /. pitch

let tick_duration_samples rate = (truncate (rate *. tick_duration)) - fade_samples

let s_count = ref 0.

let start fn =
  new_measure_fn := fn;
  next_bar ();
  inner_vol := !volume;
  period := next_period ();
  duration := tick_duration_samples !samples_per_sec;
  s_count := 0.;
  state := Tick

let two_pi = 4. *. (asin 1.)
let sine_ct = 256
let sines = let s = Array.make 256 0. in
	    for i = 0 to 255 do
	      s.(i) <- sin (two_pi *. ((float_of_int i) /. 255.));
	    done;
	    s
let sine_wave d p =
  let v = ((mod_float d p) /. (p -. 1.)) in
  sines.(min (truncate (256. *. v)) 255)

let update _ out l =
  if !state == Stopped then (Genarray.fill out 0.; 0)
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
	| Tick -> state := Fade; duration := fade_samples
	| Fade -> state := Rest;
	  let t = (snd (!measure).(!measure_p)) in
	  duration := truncate (!samples_per_sec *. (t -. tick_duration))
	| Rest ->
	  state := Tick; inner_vol := !volume; next_beat ();
	  s_count := 0.;
	  period := next_period ();
	  duration := tick_duration_samples !samples_per_sec
	| Stopped -> ());
  done;
  if !period = 0. then state := Stopped;
  0

let init () =
  Portaudio.init ();
  at_exit Portaudio.terminate;
  let try_device i =
    let outparam = { Portaudio.channels=1;
		     Portaudio.device=i;
		     Portaudio.sample_format=Portaudio.format_float32;
		     Portaudio.latency=0. } in
    try (let d = Portaudio.get_device_info i in
	 Some (Portaudio.open_stream None (Some outparam)
		 ~interleaved:true ~callback:update
		 d.Portaudio.d_default_sample_rate 16384 [],
	       d.Portaudio.d_default_sample_rate)) with
      | Portaudio.Error e -> None
      | Portaudio.Unanticipated_host_error -> None
  in
  let rec try_devices = function
    | 0 -> raise (No_device ("Tried our best to get an audio device, but failed."^
				"  Make sure jack is running."))
    | i -> match try_device (i-1) with
	     | None -> try_devices (i-1)
	     | Some (stream,rate) -> (stream, rate)
  in
  let (s,r) = try_devices (Portaudio.get_device_count ()) in
  stream := Some s; samples_per_sec := r;
  may Portaudio.start_stream !stream

let stop () =
  state := Stopped

let quit () =
  may Portaudio.stop_stream !stream
