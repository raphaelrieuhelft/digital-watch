open Unix

exception Init_ROM_file_invalid

let to_bool_array n s =
    if not(String.length s = n) then raise InvalidValue;
    let t = Array.make n false in
    for i=0 to n-1 do
      match s.[i] with
      | '0' -> ()
  	  | '1' -> t.(i) <- true
	  | _ -> raise Init_ROM_file_invalid
    done;
    t
let to_bit_array n s = VBitArray (to_bool_array n s)


let main addr_size word_size filename =
	let file_descr = openfile filename [O_RDONLY] 0o640 in
	let in_chan = in_channel_of_descr file_descr in
		
	let addr_num = 1 lsl addr_size in
	let word_len = 1 lsl word_size in
	let t = Array.make addr_num [||] in
	for i=0 to addr_num-1 do
		let s = input_char in_chan in
		t.[i] <- to_bit_array word_len s;
	done;
	t