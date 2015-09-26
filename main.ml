
let locale = GMain.Main.init ()

let midi_state = ref []
let fifth root = root *. (2.**(7./.12.))
let root_pitch = ref 440.
let meter = ref Midi.({numerator=4; denominator=4;})
let tempo = ref 120.

let construct_measure m speed_factor =
  let open Midi in
  let {numerator; denominator} = m in
  let dur = ((60. /. !tempo) /. ((float_of_int denominator) /. 4.)) /. speed_factor in
  let a = Array.make numerator (!root_pitch,dur) in
  a.(0) <- (fifth !root_pitch,dur);
  a

type preroll_box = {bars:GData.adjustment;
                    numerator_adjustment:GData.adjustment;
                    denominator_adjustment:GData.adjustment}

let make_preroll packing =
  let frame = GBin.frame ~label:"Preroll:" ~packing ~border_width:10 () in
  let hbox = GPack.hbox ~packing:frame#add ~border_width:10 () in
  let bars = GData.adjustment ~lower:0. ~value:2. ~page_size:0. () in
  let packing = (hbox#pack ~expand:false) in
  GEdit.spin_button ~adjustment:bars ~packing () |> ignore;
  GMisc.label ~text:"bars of" ~packing () |> ignore;
  let numerator_adjustment = GData.adjustment ~lower:1. ~value:4. ~page_size:0. () in
  let denominator_adjustment = GData.adjustment ~lower:1. ~value:4. ~page_size:0. () in
  let _ = GEdit.spin_button ~adjustment:numerator_adjustment ~packing () in
  let _ = GMisc.label ~text:"/" ~packing:(hbox#pack ~expand:false) () in
  let _ = GEdit.spin_button ~adjustment:denominator_adjustment ~packing () in
  {bars; numerator_adjustment; denominator_adjustment}

let make_speed_box packing =
  let frame = GBin.frame ~label:"Speed:" ~packing ~border_width:10 () in
  let hbox = GPack.hbox ~packing:frame#add () in
  let speed_factor = GData.adjustment ~lower:0.001 ~upper:2. ~value:1. ~step_incr:0.01 ~page_size:0. () in
  GRange.scale `HORIZONTAL ~value_pos:`RIGHT ~adjustment:speed_factor ~digits:3 ~packing:(hbox#pack ~expand:true ~padding:10) |> ignore;
  speed_factor

type position_labels = {key:GMisc.label; meter:GMisc.label; tempo:GMisc.label;}
let make_position_box packing =
  let hbox = GPack.hbox ~packing ~border_width:10 () in
  let position = GData.adjustment ~lower:1. ~upper:666. ~value:1. ~page_size:0. () in
  let packing = (hbox#pack ~expand:true) in
  GRange.scale `HORIZONTAL ~draw_value:false ~adjustment:position ~packing () |> ignore;
  let _ = GMisc.label ~text:"Bar:" ~packing () in
  let _ = GEdit.spin_button ~adjustment:position ~packing () in
  let packing = (hbox#pack ~expand:false ~padding:10) in
  let key = GMisc.label ~text:"(key)" ~packing () in
  let meter = GMisc.label ~text:"4/4" ~packing () in
  let tempo = GMisc.label ~text:"♩ = 120" ~packing () in
  (position, {key; meter; tempo})

let construct_file_menu menu_bar load_file =
  let file_item = GMenu.menu_item ~label:"File" ~packing:menu_bar#append () in
  let file_menu = GMenu.menu ~packing:file_item#set_submenu () in
  let packing = file_menu#append in
  let open_fn () =
    let chooser = GWindow.file_selection ~title:"Choose MIDI file" () in
    ignore (chooser#ok_button#connect#clicked ~callback:(fun () -> load_file chooser#filename; chooser#destroy ()));
    ignore (chooser#cancel_button#connect#clicked ~callback:chooser#destroy);
    chooser#show () in
  let quit_fn = GMain.Main.quit in
  List.iter (fun (l,f) -> let i = GMenu.menu_item ~label:l ~packing () in
                          ignore (i#connect#activate ~callback:f))
    [("Open", open_fn); ("Quit", quit_fn)]

let main () =
  let window = GWindow.window ~title:"Shred for Satan" () in
  ignore (window#connect#destroy ~callback:GMain.Main.quit);
  let vbox = GPack.vbox ~packing:window#add () in
  let menu_bar = GMenu.menu_bar ~packing:vbox#pack () in

  let packing = (vbox#pack ~expand:false ~padding:5) in
  let file_label = GMisc.label ~packing
      ~text:"No file loaded.  (Use Open from the File menu)" () in
  GMisc.separator `HORIZONTAL ~packing () |> ignore;

  let preroll = make_preroll packing in
  let speed_factor = make_speed_box packing in

  GMisc.separator `HORIZONTAL ~packing () |> ignore;

  let packing = (vbox#pack ~expand:false) in
  let position, labels = make_position_box packing in
  let play_btn = GButton.toggle_button ~stock:`MEDIA_PLAY ~packing () in
  play_btn#misc#set_sensitive false;

  let update_labels_for_position bar = begin
    let open Midi in
    let nearest_to_bar p blank =
      List.fold_left (fun a (m,b) -> if m <= bar && (p b) then (m,b) else a)
        (0,blank) !midi_state
    in begin
      let blank = {numerator=4;denominator=4;} in
      meter := (match
                  (nearest_to_bar (function | Meter _ -> true | _ -> false) (Meter blank))
                with | (_,Meter m) -> m | _ -> blank);
      tempo := (match (nearest_to_bar (function | Tempo _ -> true | _ -> false) (Tempo 120.))
         with | (_,Tempo t) -> t | _ -> 120.);
      let blank = (MajorKey 0) in
      let key = match (nearest_to_bar (function | Key _ -> true | _ -> false) (Key blank))
        with | (_,Key k) -> k | _ -> blank in
      root_pitch := root_pitch_of_key key;
      labels.key#set_label (pretty_print_key key);
      let {numerator;denominator} = !meter in
      labels.meter#set_label (Printf.sprintf "%d/%d" numerator denominator);
      let is_adjusted = speed_factor#value <> 1.0 in
      labels.tempo#set_label
        (Printf.sprintf "♩ = %d%s" (truncate (!tempo *. speed_factor#value))
           (if is_adjusted then (Printf.sprintf " (%d)" (truncate !tempo)) else ""))
    end
  end in
  let f () = (update_labels_for_position (truncate (position#value))) in
  ignore (position#connect#value_changed ~callback:f);

  let load_file path = begin
    midi_state := Midi.read_midi_file path;
    file_label#set_label path;
    position#set_bounds ~upper:(float_of_int (fst (List.hd !midi_state))) ();
    midi_state := List.rev !midi_state;
    position#set_value 1.;
    update_labels_for_position 1;
    play_btn#misc#set_sensitive true;
  end in

  let last_position = ref position#value in
  let f () = begin
    if play_btn#active then
      last_position := position#value
    else
      position#set_value !last_position;
    let bar_count = ref 0 in
    let next_bar = ref (truncate position#value) in
    let fresh_bar () =
      let open Midi in
      incr bar_count;
      if !bar_count <= truncate (preroll.bars#value) then
        construct_measure {numerator=(truncate (preroll.numerator_adjustment#value));
                           denominator=(truncate (preroll.denominator_adjustment#value))}
          speed_factor#value
      else begin
        position#set_value (float_of_int !next_bar);
        GtkThread.async (fun () -> update_labels_for_position (truncate position#value)) ();
        if position#value >= position#upper then begin
          play_btn#set_active false;
          [| (0.,1.) |]
        end else begin
          incr next_bar;
          construct_measure !meter speed_factor#value
        end
      end in
    (if play_btn#active then `MEDIA_STOP else `MEDIA_PLAY)
    |> GtkStock.convert_id |> play_btn#set_label;
    let f = if play_btn#active then
        (fun () -> Metronome.start fresh_bar)
      else Metronome.stop in
    GtkThread.async f ()
  end in
  ignore (play_btn#connect#clicked ~callback:f);

  construct_file_menu menu_bar load_file;

  Metronome.init ();
  window#show ();
  GtkThread.main ()

let _ = main ()
