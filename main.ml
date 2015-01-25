open Lexing
open Format

(* ajouter la production de la netlist *)



let set_file f s = f := s 

(* Les options du compilateur que l'on affiche en tapant arithc --help *)
let options = 
  ["-wait", Arg.Set Netlistprogram_simulator.wait_between_cycles, "Between cycles, wait for user to hit enter";
  (*"--parse-only", Arg.Set parse_only, 
   "  Pour ne faire uniquement que la phase d'analyse syntaxique" ; 
   "--interp", Arg.Set interp_only ,
   "  Pour interpréter au lieu de compiler" ;
   "-o", Arg.String (set_file ofile), 
   "<file>  Pour indiquer le mom du fichier de sortie"*)
   ]

let usage = ""(*"usage: minic++ [option] file.cpp"*)


let make_ast()=  
  let f = open_in Globals.code_assembleur in
    
  (* Création d'un tampon d'analyse lexicale *)
  let buf = Lexing.from_channel f in
  let p = Parser.prog Lexer.token buf in
  close_in f ;
  Precompilateur.main p
  
	
	

let main () = Arg.parse options (fun _ ->()) usage;
  (*Microprocessor_simulator.start_simulation ()*)


  
  Shared_memory.switch_input 5;
  ignore(Thread.create Tick.tick ());
  ignore(Thread.create Display.update ());
  ignore(Thread.create Inputs.handle_inputs ());
  ignore(Thread.create (fun () -> Unix.sleep 2;
    Shared_memory.switch_input 1) ());
	(*let t = make_ast() in
  Interpreteur.traite (Interpreteur.cree_tableau_inst t)*)
  Compiler.main ()


let () = main ()
