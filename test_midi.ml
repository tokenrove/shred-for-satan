open TestSimple

let () =
  plan 3;
  let midi = Midi.read_midi_file "test.midi" in
  is (List.nth midi 0) Midi.(9, EndOfTrack) "Ends after 9 beats";
  ok (match List.nth midi 1
      with | 1, Midi.Tempo t -> abs_float(t -. 180.) < 0.01
           | _ -> false) "Tempo is approximately 180";
  is (List.nth midi 2) Midi.(1, Meter {numerator=15; denominator=8}) "Meter is 15/8";
  ()
