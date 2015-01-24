 let microprocessor_inputs = Array.make Names.ninputs false
 
 let m_in = Mutex.create();
 let m_out = Mutex.create();
 let c = Condition.create();
 
 let read_inputs (f: unit->unit) = 
	Mutex.lock m_in;
	f();
	Mutex.unlock m_in;
 
 let change_output (f: unit ->unit) =
	Mutex.lock m_out;
	f();
	Condition.signal c;
	Mutex.unlock m_out
 
 
 
 let digits_RAM_count = 16
 let digits_RAM_word_size = 7
 let digits_RAM = Array.make digits_RAM_count [||]
 
 let init () =
	for i=0 to digits_RAM_count-1 do
		digits_RAM.(i) <- Array.make digits_RAM_word_size false;
	done