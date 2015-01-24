open Netlist_ast

  
let int_to_binary_string size n =
  let s = String.make size '0' in
  let rec aux i m =
    if i<0 then () else
    begin
    if m mod 2 = 1 then s.[i] <- '1';
    aux (i-1) (m/2)
    end
  in aux (size-1) n;
  s


let new_ram addr_size word_size =
  let n = 1 lsl addr_size in
  let t = Array.make n [||] in
  for i=0 to n-1 do
    t.(i) <- Array.make word_size false
  done;
  t
	
let main reg_tbl rom_tbl ram_tbl eqs =
  Hashtbl.clear reg_tbl; Hashtbl.clear rom_tbl; Hashtbl.clear ram_tbl;
  List.iter (fun (id,exp) -> match exp with
    | Ereg _ -> Hashtbl.add reg_tbl id false
    | Erom (addr_size,word_size,_) ->
	  let filename = Names.ident_to_filename id in
      Hashtbl.add rom_tbl id (Read_binary_file.main addr_size word_size filename)
    | Eram (addr_size,word_size,_,_,_,_) -> 
      Hashtbl.add ram_tbl id (new_ram addr_size word_size)
    | _ -> ()
  ) eqs
