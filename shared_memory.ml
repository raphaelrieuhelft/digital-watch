 let microprocessor_inputs = Array.make Names.ninputs false
 
 let digits_RAM_count = 16
 let digits_RAM_word_size = 7
 let digits_RAM = Array.make digits_RAM_count [||]
 
 let init () =
	for i=0 to digits_RAM_count-1 do
		digits_RAM.(i) <- Array.make digits_RAM_word_size false;
	done