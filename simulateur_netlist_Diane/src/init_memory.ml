open Netlist_ast

(*normalement on peut enlever cette fonction*)
let rec exp p x =
  if p = 0 then 1 else
  let y = exp (p/2) x in
  if p mod 2 = 0 then y*y else y*y*x
  
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
  
let rec read_cell t i word_size =
  try
    Format.printf "  cell %s" (int_to_binary_string word_size i);
    t.(i) <- Read_value.read_bool_array word_size;
  with Read_value.InvalidValue -> read_cell t i word_size
  
let read_rom id addr_size word_size =
  let n = 1 lsl addr_size in
  let t = Array.make n [||] in
  Format.printf "Initializing ROM with output %s\n" id;
  Format.printf "addr_size : %d ; word_size : %d@." addr_size word_size;
  for i=0 to n-1 do
	read_cell t i word_size;
  done;
  t

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
      Hashtbl.add rom_tbl id (read_rom id addr_size word_size)
    | Eram (addr_size,word_size,_,_,_,_) -> 
      Hashtbl.add ram_tbl id (new_ram addr_size word_size)
    | _ -> ()
  ) eqs
