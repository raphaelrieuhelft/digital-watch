open Netlist_ast
open Globals
  



let new_ram addr_size word_size =
  let n = 1 lsl addr_size in
  let t = Array.make n [||] in
  for i=0 to n-1 do
    t.(i) <- Array.make word_size false
  done;
  t
  
let init_registers reg_tbl eqs = 
  List.iter (fun (id,exp) -> match exp with
    | Ereg _ -> Hashtbl.add reg_tbl id false
    | _ -> ()
  ) eqs	
 
(* 
let init_ROMs rom_tbl eqs =
  List.iter (fun (id,exp) -> match exp with
    | Erom (addr_size,word_size,_) ->
	  let filename = ident_to_filename id in
      Hashtbl.add rom_tbl id (Read_binary_file.main addr_size word_size filename)
    | _ -> ()
  ) eqs	 
*)

let init_ROMs rom_tbl =
	Hashtbl.add rom_tbl programROM (Read_binary_file.main programROM_addr_size programROM_word_size programROM_filename);
	Hashtbl.add rom_tbl dec7 (Read_binary_file.main dec7_addr_size dec7_word_size dec7_filename)

let init_RAM ram_tbl =
  Hashtbl.add ram_tbl registersRAM (new_ram registersRAM_addr_size
  registersRAM_word_size);
  Hashtbl.add ram_tbl registersRAM2 (new_ram registersRAM_addr_size registersRAM_word_size)
 
let main reg_tbl rom_tbl ram_tbl eqs =
  Hashtbl.clear reg_tbl; Hashtbl.clear rom_tbl; Hashtbl.clear ram_tbl;
  init_registers reg_tbl eqs;
  try begin
  init_ROMs rom_tbl;
  init_RAM ram_tbl
  end
  with Not_found -> Format.eprintf "init_memory failure@."; raise Not_found
  
  (*
  List.iter (fun (id,exp) -> match exp with
    | Ereg _ -> Hashtbl.add reg_tbl id false
    | Erom (addr_size,word_size,_) ->
	  let filename = Globals.ident_to_filename id in
      Hashtbl.add rom_tbl id (Read_binary_file.main addr_size word_size filename)
    | Eram (addr_size,word_size,_,_,_,_) -> 
      Hashtbl.add ram_tbl id (new_ram addr_size word_size)
    | _ -> ()
  ) eqs
*)
