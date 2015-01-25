open Shared_memory


let wait () = Thread.delay 0.2

let force_pause () = set_input 1 false
let force_marche () = set_input 1 true

let reset () =
	force_pause ();
	wait ();
	switch_input 4;
	wait ()
	
let change_reglage_courant () = switch_input 2; wait ()
let incremente_reglage_courant () = switch_input 3; wait ()

