let ninputs = 6

let input = Array.make ninputs ""
for i=0 to ninputs-1 do
	input.(i) = "in"^(string_of_int i);
done;

let programROM = "program"
let programROM_file = "tmp/program.bin"

let dec7 = "dec7"
let dec7_file = "tmp/dec7.bin"

let ident_to_filename = function
	| s when s=programROM -> programROM_file
	| s when s=dec7 -> dec7_file
	| _ -> failwith "ident_to_filename"
	
	
let microprocessor_file = "microprocesseur/microprocesseur.net"