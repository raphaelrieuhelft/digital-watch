open Input_helpers
open Unix
let demande_temps () = Unix.localtime (Unix.gettimeofday())

let rec repete f n = match n with
    | 0 -> ()
    | n -> f() ; repete f (n-1)
	
(*let rec attend_minute () =
	let sec = (demande_temps()).tm_sec in
	if (sec>2 && sec<58) then
		(sleep 1; attend_minute ())*)

let synchro () =
    let t = demande_temps () in
    reset();
	change_reglage_courant();
    List.iter (fun n -> repete incremente_reglage_courant n ; change_reglage_courant ()) [t.tm_min ; t.tm_hour ; t.tm_year - 100 ; t.tm_mon ; t.tm_mday - 1 ] ;
	change_reglage_courant();
    let t2 = demande_temps () in
    (*Thread.delay (float_of_int(60 - (t2.Unix.tm_sec - t.Unix.tm_sec + 60 * (t2.tm_min - t.tm_min)))) ;*)
	let mn = t2.tm_min + (if t2.tm_sec>=30 then 1 else 0) in
	repete incremente_reglage_courant (mn-t.tm_min);
    force_marche()
(*	on démarre à 0 secondes dès qu'on a fini de régler, en ayant arrondi à la minute la plus proche
	donc on a une erreur d'au plus 35s environ
*)


let boucle_s tstart temps b =
    let etape1 = ref true in
    Thread.delay tstart ;
    while b || !etape1 do
        synchro ();
        etape1 := false ;
        Thread.delay temps 
    done 
