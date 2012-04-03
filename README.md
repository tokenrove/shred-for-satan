Shred for Satan
===============
A MIDI file-driven metronome
----------------------------

This is a metronome which follows the meter, tempo, and key changes in
a MIDI file.  I wrote it specifically for the needs of the guitarist
in one of my bands, where the music is filled with meter and tempo
changes, making it difficult to practice sections with a conventional
metronome.  The speed of the metronome can be changed, relative to the
tempo changes in the MIDI file.

To build and install, run:

    oasis setup
	ocaml setup.ml -build
	ocaml setup.ml -install

Please see the file COPYING for license information.
