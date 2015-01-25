open Lexing
open Format

(* ajouter la compilation du code assembleur et la production de la netlist *)






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
  Shared_memory.switch_input 5;
  ignore(Thread.create Tick.tick ());
  ignore(Thread.create Display.update ());
  ignore(Thread.create (fun () -> Unix.sleep 5;
    Shared_memory.switch_input 1) ());
  Interpreteur.traite (Interpreteur.cree_tableau_inst t)


let () = main ()
