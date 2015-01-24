open Lexing
open Format

(* ajouter la compilation du code assembleur et la production de la netlist *)



let localisation pos =
  let l = pos.pos_lnum in
  let c = pos.pos_cnum - pos.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n"
    Globals.code_assembleur (c-1) c

let doublelocal (p,q) =
  let l1 = p.pos_lnum in
  let c1 = p.pos_cnum - p.pos_bol + 1 in
  let l2 = q.pos_lnum in
  let c2 = q.pos_cnum - q.pos_bol + 1 in
	if l1 = l2 then
		eprintf "L'erreur se trouve à la ligne %d, commence au caractère %d et se finit au caractère %d :\n" l1 c1 c2	
	else eprintf "L'erreur se trouve entre les lignes %d et %d, commence au caractère %d et se finit au caractère %d :\n" l1 l2 c1 c2

let make_ast()=  
  let f = open_in Globals.code_assembleur in
    
  (* Création d'un tampon d'analyse lexicale *)
  let buf = Lexing.from_channel f in
  let p = Parser.prog Lexer.token buf in
  close_in f ;
  Precompilateur.main p
  
	
	

let main () =
  Arg.parse
    [      "-wait", Arg.Set Netlistprogram_simulator.wait_between_cycles, "Between cycles, wait for user to hit enter";
    ]
    (fun _ ->())
    ""
	;
  (*Microprocessor_simulator.start_simulation ()*)


  let t = make_ast() in
  ignore(Thread.create Display.update ());
  Interpreteur.traite (Interpreteur.cree_tableau_inst t)


let () = main ()
