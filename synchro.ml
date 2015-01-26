open Input_helpers
open Unix


let demande_temps () = Unix.localtime (Unix.gettimeofday())

let valeurs tm = [tm.tm_min ; tm.tm_hour ; tm.tm_year - 100 ; tm.tm_mon ; tm.tm_mday - 1 ]

let rec repete f n = match n with
    | 0 -> ()
    | n -> f() ; repete f (n-1)
	
(*let rec attend_minute () =
	let sec = (demande_temps()).tm_sec in
	if (sec>2 && sec<58) then
		(sleep 1; attend_minute ())*)

let synchro () =
	let t = gettimeofday () in
	let tm = localtime t in
	let tm_valeurs = valeurs tm in
	let nombre_wait_estime = (List.fold_left (fun a x -> a+x) 0 tm_valeurs) + 9 in
	let ajouter_minute = delais*.(float_of_int nombre_wait_estime)+.(float_of_int tm.tm_sec) >= 30. in
	let tm_valeurs = if ajouter_minute then valeurs (localtime (t+.60.)) else tm_valeurs in
    reset();
	change_reglage_courant();
    List.iter (fun n -> repete incremente_reglage_courant n ; change_reglage_courant ()) tm_valeurs;
	(*change_reglage_courant();*)
    (*let t2 = demande_temps () in*)
    (*Thread.delay (float_of_int(60 - (t2.Unix.tm_sec - t.Unix.tm_sec + 60 * (t2.tm_min - t.tm_min)))) ;*)
	(*let mn = t2.tm_min + (if t2.tm_sec>=30 then 1 else 0) in
	repete incremente_reglage_courant (mn-t.tm_min);*)
    force_marche()
(*	on démarre à 0 seconde dès qu'on a fini de régler, en ayant arrondi à la minute la plus proche estimée *)


let boucle_s tstart temps b =
    let etape1 = ref true in
    Thread.delay tstart ;
    while b || !etape1 do
        synchro ();
        etape1 := false ;
        Thread.delay temps 
    done 
