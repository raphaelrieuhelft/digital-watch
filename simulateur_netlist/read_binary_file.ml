open Unix
open Netlist_ast
exception Init_ROM_file_invalid


let to_bool_array n s =
    if not(String.length s = n) then raise Init_ROM_file_invalid;
    let t = Array.make n false in
    for i=0 to n-1 do
      match s.[i] with
      | '0' -> ()
  	  | '1' -> t.(i) <- true
	  | _ -> raise Init_ROM_file_invalid
    done;
    t


let main addr_size word_size filename =
	let file_descr = openfile filename [O_RDONLY] 0o640 in
	let in_chan = in_channel_of_descr file_descr in
		
	let addr_count = 1 lsl addr_size in
	let t = Array.make addr_count [||] in
	for i=0 to addr_count-1 do
		let s = input_line in_chan in
		t.(i) <- to_bool_array word_size s;
	done;
	t
