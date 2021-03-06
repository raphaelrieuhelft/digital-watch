 open Globals
 
 let microprocessor_inputs = Array.make ninputs false
 let digitsRAM_count = 1 lsl digitsRAM_addr_size
 let digits_RAM = Array.make digitsRAM_count [||]
 
 let () =
	for i=0 to digitsRAM_count-1 do
		digits_RAM.(i) <- Array.make digitsRAM_word_size false;
	done
	
	
 let m_in = Mutex.create()
 let m_out = Mutex.create()
 let c = Condition.create()
 
 let switch_input i =
   Mutex.lock m_in;
   microprocessor_inputs.(i) <- not microprocessor_inputs.(i);
   Mutex.unlock m_in
   
let set_input i b =
	Mutex.lock m_in;
	microprocessor_inputs.(i) <- b;
	Mutex.unlock m_in
 
 let get_inputs () = 
	Mutex.lock m_in;
	let inputs = Array.copy microprocessor_inputs in
	Mutex.unlock m_in;
	inputs
 
 let write_in_digitsRAM addr data =
	Mutex.lock m_out;
	digits_RAM.(addr) <- data;
	Mutex.unlock m_out;
	Condition.signal c 
 
 

