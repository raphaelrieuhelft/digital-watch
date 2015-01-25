open Lexing
open Format

(* ajouter la production de la netlist *)

let set_file f s = f := s 

(* Les options du compilateur que l'on affiche en tapant arithc --help *)
let options = 
  [ "-wait", Arg.Set Netlistprogram_simulator.wait_between_cycles, "Between cycles, wait for user to hit enter";
	"--no-compile", Arg.Clear Globals.compile, "Pour ne faire pas compiler le code assembleur (un fichier "^Globals.programROM_filename^" doit exister)" ; 
	"--parse-only", Arg.Set Globals.parse_only, "Pour ne faire que la phase d'analyse syntaxique" ; 
	"--print-precompiled", Arg.Set Globals.print_precompiled, "Imprime le code assembleur précompilé" ;
	"--interp", Arg.Set Globals.interp_only , "Pour interpréter au lieu de compiler" ;
	"-i", Arg.Set_string Globals.compiler_source_filename, "<file>  Pour indiquer le mom du fichier à compiler (par défaut "^Globals.code_assembleur^" )";
	"-o", Arg.Set_string Globals.compiler_out_filename, "<file>  Pour indiquer le mom du fichier de sortie du compilateur"
   ]

let usage = ""(*"usage: minic++ [option] file.cpp"*)


let temps_synchro = 5.0


	

let main () = Arg.parse options (fun _ ->()) usage;
  let p = Compiler.precompile () in
  if !Globals.compile then Compiler.compile p;
  Shared_memory.switch_input 5;
  ignore(Thread.create Tick.tick ());
  ignore(Thread.create Display.update ());
  ignore(Thread.create Inputs.handle_inputs ());
  ignore(Thread.create (fun () -> Unix.sleep 2;
    Shared_memory.switch_input 1) ());
  if !Globals.interp_only
  then Compiler.interp p
  else 
    let t = Thread.create Microprocessor_simulator.start_simulation () in 
    ignore(Thread.create (fun () -> Synchro.boucle_s temps_synchro) ());
        Thread.join t


let () = main ()
