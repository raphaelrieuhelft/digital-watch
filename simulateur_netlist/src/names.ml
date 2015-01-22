let ninputs = 6

let input = Array.make ninputs ""
for i=0 to ninputs-1 do
	input.(i) = "in"^(string_of_int i);
done;

let programROM = "program"

let programROM_file = "netlist/program.txt"