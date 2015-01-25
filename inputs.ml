open Graphics
open Shared_memory

let force_pause () = failwith "non fait"
let force_marche () = failwith "non fait"

let reset () =
	force_pause ();
	failwith "non fait"
	
let change_reglage_courant () = failwith "non fait"
let incremente_reglage_courant () = failwith "non fait"



let on_input c = match c with
  |'p'|'\r' -> switch_input 1
  |'\t' -> switch_input 2
  |' ' ->switch_input 3
  |'\b'->switch_input 4
  |'s' ->switch_input 5
  |_->()

let handle_inputs ()=
  while true do
    on_input(read_key());
    Thread.yield();
  done
    
