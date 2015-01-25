open Shared_memory

let delais = 0.3
let wait () = Thread.delay delais

let force_pause () = set_input 1 false
let force_marche () = set_input 1 true

let reset () =
	force_pause ();
	wait ();
	switch_input 4;
	wait ()
	
let change_reglage_courant () = switch_input 2; wait ()
let incremente_reglage_courant () = switch_input 3; wait ()

