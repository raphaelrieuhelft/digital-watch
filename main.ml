


(* ajouter la compilation du code assembleur et la proction de la netlist *)



let compile filename = ()


	
	

let main () =
  Arg.parse
    [      "-wait", Arg.Set Netlistprogram_simulator.wait_between_cycles, "Between cycles, wait for user to hit enter";
    ]
    compile
    ""
	;
  Microprocessor_simulator.start_simulation ()


let () = main ()
