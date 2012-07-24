(* Copyright 2011 Julian Squires <julian@cipht.net>
 * Please see the COPYING file for license information. *)

let locale = GMain.Main.init ()

let midi_state = ref []
let fifth root = root *. (2.**(7./.12.))
let root_pitch = ref 440.
let meter = ref {Midi.numerator=4; Midi.denominator=4;}
let tempo = ref 120.

let quit () =
  Metronome.quit ();
  GMain.Main.quit ();;

let recent_files = ref []
let recent_update_hook = ref (fun () -> ())

let rec keep n l = if n < 1 then []
  else match l with | hd::tl -> hd::(keep (n-1) tl) | [] -> []

(* Add p to the recent list.  If p is already in recent, move it to
   the top.  If there are more than n items on the list, drop all but
   n. *)
let add_to_recent p =
  let n = 5 in
  if List.mem p !recent_files then
    recent_files := p :: (List.filter (fun x -> x <> p) !recent_files)
  else
    recent_files := p :: (keep (n-1) !recent_files);
  (!recent_update_hook)();;

let recent_files_file = Filename.concat (Sys.getenv "HOME") ".shred-for-satan_recent"

let load_recent () =
  try begin
    let c = open_in_bin recent_files_file in
    recent_files := Marshal.from_channel c;
    close_in_noerr c
  end with | Sys_error s ->
    (* recent_files is already [] by default *)
    Printf.fprintf stderr "Failed to open %s: %s\n" recent_files_file s;;

let save_recent () =
  try begin
    let c = open_out_bin recent_files_file in
    Marshal.to_channel c (!recent_files) [];
    close_out c
  end with | Sys_error s ->
    Printf.fprintf stderr "Failed to save %s: %s\n" recent_files_file s;;

let simple_menu_item r l f =
  let i = GMenu.menu_item ~label:l ~packing:r#append () in
  ignore (i#connect#activate ~callback:f);;

let main () =
  let window = GWindow.window ~title:"Shred for Satan" () in
  ignore (window#connect#destroy ~callback:quit);
  let vbox = GPack.vbox ~packing:window#add () in
  let menu_bar = GMenu.menu_bar ~packing:vbox#pack () in
  let file_item = GMenu.menu_item ~label:"File" ~packing:menu_bar#append () in
  let file_menu = GMenu.menu ~packing:file_item#set_submenu () in

  (* File label *)
  let file_label = GMisc.label ~text:"No file loaded.  (Use Open from the File menu)"
    ~packing:(vbox#pack ~expand:false) () in

  let _ = GMisc.separator `HORIZONTAL ~packing:(vbox#pack ~expand:false ~padding:5) () in

  (* Preroll *)
  let frame = GBin.frame ~label:"Preroll:" ~packing:(vbox#pack ~expand:false ~padding:5) ~border_width:10 () in
  let hbox = GPack.hbox ~packing:frame#add ~border_width:10 () in
  let preroll_arrow = GMisc.arrow ~show:false ~packing:(hbox#pack ~expand:false) ~kind:`RIGHT () in
  let preroll_bars = GData.adjustment ~lower:0. ~value:2. ~page_size:0. () in
  let _ = GEdit.spin_button ~adjustment:preroll_bars ~packing:(hbox#pack ~expand:false) () in
  let _ = GMisc.label ~text:"bars of" ~packing:(hbox#pack ~expand:false) () in
  let preroll_numerator = GData.adjustment ~lower:1. ~value:4. ~page_size:0. () in
  let preroll_denominator = GData.adjustment ~lower:1. ~value:4. ~page_size:0. () in
  let _ = GEdit.spin_button ~adjustment:preroll_numerator ~packing:(hbox#pack ~expand:false) () in
  let _ = GMisc.label ~text:"/" ~packing:(hbox#pack ~expand:false) () in
  let _ = GEdit.spin_button ~adjustment:preroll_denominator ~packing:(hbox#pack ~expand:false) () in

  (* Position bar *)
  let hbox = GPack.hbox ~packing:(vbox#pack ~expand:false) ~border_width:10 () in
  let position_arrow = GMisc.arrow ~show:false ~packing:(hbox#pack ~expand:false) ~kind:`RIGHT () in
  let position = GData.adjustment ~lower:1. ~upper:666. ~value:1. ~page_size:0. () in
  let position_bar = GRange.scale `HORIZONTAL ~draw_value:false ~adjustment:position ~packing:(hbox#pack ~expand:true) () in
  let _ = GMisc.label ~text:"Bar:" ~packing:(hbox#pack ~expand:false) () in
  let position_box = GEdit.spin_button ~adjustment:position ~packing:(hbox#pack ~expand:false) () in
  let key_label = GMisc.label ~text:"(key)" ~packing:(hbox#pack ~expand:false ~padding:10) () in
  let meter_label = GMisc.label ~text:"4/4" ~packing:(hbox#pack ~expand:false ~padding:10) () in
  let tempo_label = GMisc.label ~text:"♩ = 120" ~packing:(hbox#pack ~expand:false ~padding:10) () in

  let _ = GMisc.separator `HORIZONTAL ~packing:(vbox#pack ~expand:false ~padding:5) () in

  (* Speed adjustment *)
  let frame = GBin.frame ~label:"Speed:" ~packing:(vbox#pack ~expand:false ~padding:5) ~border_width:10 () in
  let hbox = GPack.hbox ~packing:frame#add () in
  let speed_factor = GData.adjustment ~lower:0.001 ~upper:2. ~value:1. ~step_incr:0.01 ~page_size:0. () in
  let _ = GRange.scale `HORIZONTAL ~value_pos:`RIGHT ~adjustment:speed_factor ~digits:3 ~packing:(hbox#pack ~expand:true ~padding:10) () in
  let speed_reset = GButton.button ~label:"Reset" ~packing:(hbox#pack ~expand:false) () in
  ignore (speed_reset#connect#clicked ~callback:(fun () -> speed_factor#set_value 1.));

  (* Play button *)
  let play_btn = GButton.toggle_button ~stock:`MEDIA_PLAY ~packing:(vbox#pack ~expand:false) () in
  play_btn#misc#set_sensitive false;

  let update_labels_for_position bar = begin
    let nearest_to_bar p blank = List.fold_left (fun a (m,b) -> if m <= bar && (p b) then (m,b) else a) (0,blank) !midi_state in begin
      let blank = {Midi.numerator=4;Midi.denominator=4;} in
      meter := (match
	  (nearest_to_bar (function | Midi.Meter _ -> true | _ -> false) (Midi.Meter blank))
	with | (_,Midi.Meter m) -> m | _ -> blank);
      tempo := (match (nearest_to_bar (function | Midi.Tempo _ -> true | _ -> false) (Midi.Tempo 120.))
	with | (_,Midi.Tempo t) -> t | _ -> 120.);
      let blank = (Midi.MajorKey 0) in
      let key = (match (nearest_to_bar (function | Midi.Key _ -> true | _ -> false) (Midi.Key blank))
	with | (_,Midi.Key k) -> k | _ -> blank) in
      root_pitch := Midi.root_pitch_of_key key;
      key_label#set_label (Midi.pretty_print_key key);
      meter_label#set_label (Midi.string_of_meter !meter);
      let l = if speed_factor#value <> 1.0 then
	  ("♩ = "^ (string_of_int (truncate (!tempo *. speed_factor#value))) ^ " ("^ (string_of_int (truncate !tempo))^")")
	else
	  ("♩ = "^ (string_of_int (truncate !tempo)))
      in
      tempo_label#set_label l
    end
  end in

  ignore (position#connect#value_changed
	    ~callback:(fun () -> update_labels_for_position (truncate (position#value))));

  let enter_playing_state playingp =
    play_btn#set_label (GtkStock.convert_id (if playingp then `MEDIA_STOP else `MEDIA_PLAY));
    List.iter (fun x -> x#misc#hide ()) [preroll_arrow; position_arrow];
    List.iter (fun x -> x#set_sensitive (not playingp)) [position_bar#misc; position_box#misc];
  in

  let load_file path = (try begin
    play_btn#set_active false;
    enter_playing_state false;
    Metronome.stop ();
    midi_state := Midi.read_midi_file path;
    file_label#set_label path;
    position#set_bounds ~upper:(float_of_int (fst (List.hd !midi_state))) ();
    midi_state := List.rev !midi_state;
    position#set_value 1.;
    update_labels_for_position 1;
    add_to_recent path;
    play_btn#misc#set_sensitive true;
  end with | Sys_error e -> file_label#set_label e;
    play_btn#misc#set_sensitive false;) in

  let construct_measure num denom =
    let dur = ((60. /. !tempo) /. ((float_of_int denom) /. 4.)) /. speed_factor#value in
    let a = Array.make num (!root_pitch,dur) in
    a.(0) <- (fifth !root_pitch,dur);
    a
  in
  let last_position = ref position#value in
  let _ = play_btn#connect#clicked ~callback:begin fun () ->
    enter_playing_state (play_btn#active);
    if play_btn#active then last_position := position#value else position#set_value !last_position;
    let bar_count = ref 0 in
    let next_bar = ref (truncate position#value) in
    let fresh_bar () = begin
      incr bar_count;
      if !bar_count <= truncate (preroll_bars#value) then begin
	preroll_arrow#misc#show ();
	construct_measure (truncate (preroll_numerator#value)) (truncate (preroll_denominator#value))
      end else begin
	preroll_arrow#misc#hide ();
	position_arrow#misc#show ();
	position#set_value (float_of_int !next_bar);
	if position#value >= position#upper then begin
	  play_btn#set_active false;
	  [| (0.,1.) |]
	end else (incr next_bar; construct_measure (!meter).Midi.numerator (!meter).Midi.denominator)
      end
    end in
    GtkThread.async (if play_btn#active then (fun () -> Metronome.start fresh_bar) else Metronome.stop) ()
  end in

  simple_menu_item file_menu "Open" (fun () ->
    let chooser = GWindow.file_selection ~title:"Choose MIDI file" () in
    ignore (chooser#ok_button#connect#clicked
	      ~callback:(fun () -> load_file chooser#filename; chooser#destroy ()));
    ignore (chooser#cancel_button#connect#clicked ~callback:chooser#destroy);
    chooser#show ());
  let recent_item = GMenu.menu_item ~label:"Recent" ~packing:file_menu#append () in
  let recent_menu = GMenu.menu ~packing:recent_item#set_submenu () in
  simple_menu_item file_menu "Quit" quit;

  load_recent ();
  at_exit save_recent;
  let populate_recent_menu m =
    List.iter m#remove (m#all_children);
    List.iter (fun p -> simple_menu_item m (Filename.basename p) (fun () -> load_file p)) !recent_files
  in
  recent_update_hook := (fun () -> populate_recent_menu recent_menu);
  (!recent_update_hook) ();

  (try Metronome.init () with
    | Metronome.No_device s ->
      begin
  	vbox#destroy ();
  	let vbox = GPack.vbox ~packing:window#add () in
  	let _ = GMisc.label ~text:s ~packing:(vbox#pack ~expand:false) () in
  	let b = GButton.button ~label:"Quit" ~packing:(vbox#pack ~expand:false) () in
  	ignore (b#connect#clicked ~callback:quit)
      end);

  window#show ();
  GtkThread.main ()

let _ = main ()
