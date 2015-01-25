open Graphics
open Shared_memory

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
    
