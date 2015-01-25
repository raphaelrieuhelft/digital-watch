let interval = 1./.1024.
(*let interval = 1.*)

let compte = ref 0

let tick ()=
  let rec wait_and_tick next_tick =
    while Unix.gettimeofday()<next_tick do
      Thread.yield ();
    done;
    Shared_memory.switch_input 0;
    incr compte;
    (*Format.printf "Temps système : %f@." (Unix.gettimeofday());*)
    if !compte=1024 then (
      compte:=0;
      Format.eprintf "Temps système : %f@." (Unix.gettimeofday())
    );
    wait_and_tick (next_tick+.interval)
  in wait_and_tick (Unix.gettimeofday())
