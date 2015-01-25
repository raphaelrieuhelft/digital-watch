open Graphics
open Shared_memory
open Input_helpers

let temps_synchro = 60.0
let tstart = 0.0

let on_input c = match c with
  |'\r' -> force_marche ()
  |'p' -> force_pause ()
  |'\t' -> change_reglage_courant ()
  |' ' -> incremente_reglage_courant ()
  |'\b'-> reset ()
  |'s' -> switch_input 5
  |'t' -> Synchro.boucle_s tstart temps_synchro false
  |_->()

let handle_inputs ()=
  while true do
    on_input(read_key());
    Thread.yield();
  done
    
