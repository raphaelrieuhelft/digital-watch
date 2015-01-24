open Globals

let print_scheduled = ref false

let start_simulation () =
	try
		let filename = microprocessor_filename in
		let p = Netlist_analyser.read_file filename in
		let p = Scheduler.schedule p in
		if !print_scheduled then
			begin
			let out_name = (Filename.chop_suffix filename ".net") ^ "_sch.net" in
			let out = open_out out_name in
			try
			Netlist_printer.print_program out p;
			close_out out
			with exn -> close_out out; raise exn
			end;
		Netlistprogram_simulator.simulate p
    with
		| Scheduler.Combinational_cycle ->
		  Format.eprintf "The netlist has a combinatory cycle.@.";
	      exit 2
		| Netlist_analyser.Parse_error s -> Format.eprintf "An error accurred: %s@." s; exit 2
		| exn -> raise exn
