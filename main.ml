


(* ajouter la compilation du code assembleur et la proction de la netlist *)



let compile filename = ()


	
	

let main () =
  Arg.parse
    ["-print", Arg.Set print_only, "Only print the result of scheduling";
     "-n", Arg.Set_int number_steps, "Number of steps to simulate";
     "-rand", Arg.Set Read_value.random, "Generates random entries instead of asking user";
     "-wait", Arg.Set Netlist_interp.wait_between_cycles, "Between cycles, wait for user to hit enter";
    ]
    compile
    ""
	;
  Microprocessor_simulator.start_simulation


main ()
