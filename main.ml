
let locale = GMain.Main.init ()

let midi_state = ref []
let fifth root = root *. (2.**(7./.12.))
let root_pitch = ref 440.
let meter = ref {Midi.numerator=4; Midi.denominator=4;}
let tempo = ref 120.

let main () =
  let window = GWindow.window ~title:"Shred for Satan" () in
  ignore (window#connect#destroy ~callback:GMain.Main.quit);
  let vbox = GPack.vbox ~packing:window#add () in
  let menu_bar = GMenu.menu_bar ~packing:vbox#pack () in
  let file_item = GMenu.menu_item ~label:"File" ~packing:menu_bar#append () in
  let file_menu = GMenu.menu ~packing:file_item#set_submenu () in

  let file_label = GMisc.label ~text:"No file loaded.  (Use Open from the File menu)"
    ~packing:(vbox#pack ~expand:false) () in
  let _ = GMisc.separator `HORIZONTAL ~packing:(vbox#pack ~expand:false ~padding:5) () in

  let frame = GBin.frame ~label:"Preroll:" ~packing:(vbox#pack ~expand:false ~padding:5) ~border_width:10 () in
  let hbox = GPack.hbox ~packing:frame#add ~border_width:10 () in
  let preroll_bars = GData.adjustment ~lower:0. ~value:2. ~page_size:0. () in
  let _ = GEdit.spin_button ~adjustment:preroll_bars ~packing:(hbox#pack ~expand:false) () in
  let _ = GMisc.label ~text:"bars of" ~packing:(hbox#pack ~expand:false) () in
  let preroll_numerator = GData.adjustment ~lower:1. ~value:4. ~page_size:0. () in
  let preroll_denominator = GData.adjustment ~lower:1. ~value:4. ~page_size:0. () in
  let _ = GEdit.spin_button ~adjustment:preroll_numerator ~packing:(hbox#pack ~expand:false) () in
  let _ = GMisc.label ~text:"/" ~packing:(hbox#pack ~expand:false) () in
  let _ = GEdit.spin_button ~adjustment:preroll_denominator ~packing:(hbox#pack ~expand:false) () in

  let frame = GBin.frame ~label:"Speed:" ~packing:(vbox#pack ~expand:false ~padding:5) ~border_width:10 () in
  let hbox = GPack.hbox ~packing:frame#add () in
  let speed_factor = GData.adjustment ~lower:0.001 ~upper:2. ~value:1. ~step_incr:0.01 ~page_size:0. () in
  let _ = GRange.scale `HORIZONTAL ~value_pos:`RIGHT ~adjustment:speed_factor ~digits:3 ~packing:(hbox#pack ~expand:true ~padding:10) () in

  let _ = GMisc.separator `HORIZONTAL ~packing:(vbox#pack ~expand:false ~padding:5) () in

  let hbox = GPack.hbox ~packing:(vbox#pack ~expand:false) ~border_width:10 () in
  let position = GData.adjustment ~lower:1. ~upper:666. ~value:1. ~page_size:0. () in
  let _ = GRange.scale `HORIZONTAL ~draw_value:false ~adjustment:position ~packing:(hbox#pack ~expand:true) () in
  let _ = GMisc.label ~text:"Bar:" ~packing:(hbox#pack ~expand:false) () in
  let _ = GEdit.spin_button ~adjustment:position ~packing:(hbox#pack ~expand:false) () in
  let key_label = GMisc.label ~text:"(key)" ~packing:(hbox#pack ~expand:false ~padding:10) () in
  let meter_label = GMisc.label ~text:"4/4" ~packing:(hbox#pack ~expand:false ~padding:10) () in
  let tempo_label = GMisc.label ~text:"♩ = 120" ~packing:(hbox#pack ~expand:false ~padding:10) () in

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
      meter_label#set_label ((string_of_int (!meter).Midi.numerator)^"/"^(string_of_int (!meter).Midi.denominator));
      let l = if speed_factor#value <> 1.0 then
	  ("♩ = "^ (string_of_int (truncate (!tempo *. speed_factor#value))) ^ " ("^ (string_of_int (truncate !tempo))^")")
	else
	  ("♩ = "^ (string_of_int (truncate !tempo)))
      in
      tempo_label#set_label l
    end
  end in

  ignore (position#connect#value_changed ~callback:(fun () -> update_labels_for_position (truncate (position#value))));

  let load_file path = begin
    midi_state := Midi.read_midi_file path;
    file_label#set_label path;
    position#set_bounds ~upper:(float_of_int (fst (List.hd !midi_state))) ();
    midi_state := List.rev !midi_state;
    position#set_value 1.;
    update_labels_for_position 1;
    play_btn#misc#set_sensitive true;
  end in

  let construct_measure num denom =
    let dur = ((60. /. !tempo) /. ((float_of_int denom) /. 4.)) /. speed_factor#value in
    let a = Array.make num (!root_pitch,dur) in
    a.(0) <- (fifth !root_pitch,dur);
    a
    in
  ignore (play_btn#connect#clicked ~callback:(fun () ->
    let bar_count = ref 0 in
    let next_bar = ref (truncate position#value) in
    let fresh_bar () = begin
      incr bar_count;
      if !bar_count <= truncate (preroll_bars#value) then begin
	construct_measure (truncate (preroll_numerator#value)) (truncate (preroll_denominator#value))
      end else begin
	position#set_value (float_of_int !next_bar);
	GtkThread.async (fun () -> update_labels_for_position (truncate position#value)) ();
	if position#value >= position#upper then begin
	  play_btn#set_active false;
	  [| (0.,1.) |]
	end else (incr next_bar; construct_measure (!meter).Midi.numerator (!meter).Midi.denominator)
      end
    end in
    play_btn#set_label (GtkStock.convert_id (if play_btn#active then `MEDIA_STOP else `MEDIA_PLAY));
    GtkThread.async (if play_btn#active then (fun () -> Metronome.start fresh_bar) else Metronome.stop) ()));

  List.iter (fun (l,f) -> let i = GMenu.menu_item ~label:l ~packing:file_menu#append () in
                          ignore (i#connect#activate ~callback:f); ())
    [("Open",(fun () ->
      let chooser = GWindow.file_selection ~title:"Choose MIDI file" () in
      ignore (chooser#ok_button#connect#clicked ~callback:(fun () -> load_file chooser#filename; chooser#destroy ()));
      ignore (chooser#cancel_button#connect#clicked ~callback:chooser#destroy);
      chooser#show ()));
     ("Quit", GMain.Main.quit)];

  Metronome.init ();
  window#show ();
  GtkThread.main ()

let _ = main ()
