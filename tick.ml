(*let interval = 1./.1024.*)
let interval = 1.

let compte = ref 0

let tick ()=
  let rec wait_and_tick next_tick =
    while Sys.time()<next_tick do
      Thread.yield ();
    done;
    Shared_memory.switch_input 0;
    incr compte;
    if !compte=1024 then (
      compte:=0;
      (*Format.printf "\ntick@."*)
    );
    wait_and_tick (next_tick+.interval)
  in wait_and_tick (Sys.time())
