let ninputs = 6

let input_string_to_int = function
	| "in0" -> 0
	| "in1" -> 1
	| "in2" -> 2
	| "in3" -> 3
	| "in4" -> 4
	| "in5" -> 5
	| _ -> failwith "Globals.input_string_to_int"
(*
let input = Array.make ninputs ""
for i=0 to ninputs-1 do
	input.(i) = "in"^(string_of_int i);
done;
*)

let programROM = "program"
let programROM_filename = "bin/program.bin"
let programROM_addr_size = 9
let programROM_word_size = 16

let dec7 = "dec7"
let dec7_filename = "bin/dec7.bin"
let dec7_addr_size = 4
let dec7_word_size = 7

let registersRAM = "register"
let registersRAM_addr_size = 5
let registersRAM_word_size = 12

let digitsRAM = "digits"
let digitsRAM_addr_size = 4
let digitsRAM_word_size = 7

let ident_to_filename = function
	| s when s=programROM -> programROM_filename
	| s when s=dec7 -> dec7_filename
	| _ -> failwith "ident_to_filename"
	
	
let microprocessor_filename = "microprocesseur/microprocesseur.net"