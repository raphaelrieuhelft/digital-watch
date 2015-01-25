open Inputs

let demande_temps () = Unix.localtime (Unix.gettimeofday())

let rec repete f n = match n with
    | 0 -> ()
    | n -> f() ; repete f (n-1)

let synchro () =
    let t = demande_temps () in
        reset();
        List.iter (fun n -> repete incremente_reglage_courant n ; change_reglage_courant ()) [t.Unix.tm_sec ; t.tm_min + 1 ; t.tm_hour ; t.tm_mday - 1 ; t.tm_mon ; t.tm_year - 100 ] ;
        let t2 = demande_temps () in
            Thread.delay (float_of_int(60 - (t2.Unix.tm_sec - t.Unix.tm_sec + 60 * (t2.tm_min - t.tm_min)))) ;
            force_marche() 


let boucle_s temps =
    while true do
        Thread.delay temps ;
        synchro ();
        Thread.yield()
    done 
