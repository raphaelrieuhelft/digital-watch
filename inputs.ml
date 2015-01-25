open Graphics
open Shared_memory

let wait () = Thread.delay 0.5

let force_pause () = set_input 1 false
let force_marche () = set_input 1 true

let reset () =
	force_pause ();
	wait ();
	switch_input 4;
	wait ()
	
let change_reglage_courant () = switch_input 2; wait ()
let incremente_reglage_courant () = switch_input 3; wait ()



let on_input c = match c with
  |'\r' -> force_marche ()
  |'p' -> force_pause ()
  |'\t' -> change_reglage_courant ()
  |' ' -> incremente_reglage_courant ()
  |'\b'-> reset ()
  |'s' -> switch_input 5
  |_->()

let handle_inputs ()=
  while true do
    on_input(read_key());
    Thread.yield();
  done
    
