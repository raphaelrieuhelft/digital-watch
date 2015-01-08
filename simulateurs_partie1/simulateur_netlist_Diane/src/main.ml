let print_only = ref false
let number_steps = ref (-1)

let compile filename =
  try
    let p = Netlist.read_file filename in
    let out_name = (Filename.chop_suffix filename ".net") ^ "_sch.net" in
    let out = open_out out_name in
    let close_all () = close_out out in
    begin
    try
      let p = Scheduler.schedule p in
      Netlist_printer.print_program out p;
      if not !print_only then
	    Netlist_interp.main p (!number_steps);
      close_all ()
    with exn -> close_all (); match exn with
      | Scheduler.Combinational_cycle ->
	    Format.eprintf "The netlist has a combinatory cycle.@.";
	    exit 2
      | _ -> raise exn
    end
  with
    | Netlist.Parse_error s -> Format.eprintf "An error accurred: %s@." s; exit 2
	
	

let main () =
  Arg.parse
    ["-print", Arg.Set print_only, "Only print the result of scheduling";
     "-n", Arg.Set_int number_steps, "Number of steps to simulate";
     "-rand", Arg.Set Read_value.random, "Generates random entries instead of asking user";
     "-wait", Arg.Set Netlist_interp.wait_between_cycles, "Between cycles, wait for user to hit enter";
    ]
    compile
    ""
;;

main ()
