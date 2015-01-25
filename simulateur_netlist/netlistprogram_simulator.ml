(* simulate a netlist_ast.program which should be already scheduled *)

open Globals
open Netlist_ast
open Netlist_utilities

let wait_between_cycles = ref false



let reg_tbl : ((ident,bool) Hashtbl.t) = Hashtbl.create 7
let rom_tbl : ((ident,(bool array) array) Hashtbl.t) = Hashtbl.create 7
let ram_tbl : ((ident,(bool array) array) Hashtbl.t) = Hashtbl.create 7


  
let eval_arg env = function
  | Avar id -> Env.find id env
  | Aconst v -> v

let eval_arg_to_bool env arg =
  try bool_of_value (eval_arg env arg)
  with Failure "bool_of_value" ->
    Format.eprintf "Typing error about arg:  %!";
    Netlist_printer.print_arg Format.err_formatter arg;
    Format.eprintf "@.";
    assert false
	
let eval_arg_to_array env arg = array_of_value (eval_arg env arg)


let eval_exp env id = function
  | Earg arg -> eval_arg env arg
  | Ereg _ -> (try VBit (Hashtbl.find reg_tbl id) with Not_found ->
    Format.eprintf "flip-flop not found : %s@." id; raise Not_found)
  | Enot arg -> VBit (not (eval_arg_to_bool env arg))
  | Ebinop (op,arg1,arg2) ->
    let b1 = eval_arg_to_bool env arg1 and b2 = eval_arg_to_bool env arg2 in
	let b = match op with   Or->b1||b2 | Xor->(b1<>b2) 
	                      | And->b1&&b2 | Nand->not(b1&&b2)
    in VBit b
  | Emux (arg1,arg2,arg3) -> (* si arg1 alors arg2 sinon arg3 *)
    if (eval_arg_to_bool env arg1) then eval_arg env arg2 else eval_arg env arg3
  | Erom (_,_,arg) -> 
    let t = eval_arg_to_array env arg in
	let addr = bits_to_int t in
	(try let rom = Hashtbl.find rom_tbl id in
	VBitArray (rom.(addr))
	with Not_found -> Format.eprintf "ROM not found : %s@." id;
	  raise Not_found)
  | Eram (_,_,ra_arg,we_arg,wa_arg,arg) ->
    if id=digitsRAM then VBitArray(Array.make digitsRAM_word_size false) else (*value read in digitsRAM is unused in netlist*)
      let ram = try Hashtbl.find ram_tbl id with Not_found ->
	Format.eprintf "RAM not found : %s@." id; assert false  in
      let ra_t = eval_arg_to_array env ra_arg in
      let r_addr = bits_to_int ra_t in
      let t = ram.(r_addr) in
      VBitArray t
  | Econcat (arg1,arg2) ->
    let t1 = eval_arg_to_array env arg1 and t2 = eval_arg_to_array env arg2 in
    VBitArray (Array.append t1 t2)
  | Eslice (n1,n2, arg) ->
    let t = eval_arg_to_array env arg in
    VBitArray (Array.sub t n1 (n2-n1+1))
  | Eselect (n,arg) ->
    let t = eval_arg_to_array env arg in
	VBit t.(n)
  
  
let compute_eq env_ty env (id,exp) =
  let v = eval_exp env id exp in
  try
    let v1 = value_to_value_with_ty v (Env.find id env_ty) in
    Env.add id v1 env
  with exn ->
    Format.eprintf "Error in compute_eq with eq:  %!";
    Netlist_printer.print_eq Format.err_formatter (id,exp);
    raise exn

	
let write_eq env (id,exp) = try match exp with
  | Ereg id1 -> 
    let b = bool_of_value (Env.find id1 env) in
    Hashtbl.replace reg_tbl id b
  | Eram (_,_,_,we_arg,wa_arg,arg) ->
	let we = eval_arg_to_bool env we_arg in
	if we then
	  let wa_t = eval_arg_to_array env wa_arg in
	  let w_addr = bits_to_int wa_t in
	  let data = eval_arg_to_array env arg in
	  if id=digitsRAM then Shared_memory.write_in_digitsRAM w_addr data
	  else
	    let ram = try Hashtbl.find ram_tbl id with Not_found ->
	      Format.eprintf "RAM not found : %s@." id; assert false in
	    ram.(w_addr) <- data
  | Earg _ | Enot _ | Ebinop _ | Emux _ | Erom _ | Econcat _ | Eslice _ | Eselect _
    -> ()
  with exn ->
    Format.eprintf "Error in write_eq with eq:  %!";
    Netlist_printer.print_eq Format.err_formatter (id,exp);
    raise exn

let read_input inputs id =
	let i = input_string_to_int id in
	VBit inputs.(i)
	
(*let print_output id v =
  Format.printf "  output %s : %s@." id (value_to_string v)*)
  
let print_infos env =
  try
	Format.printf "  instruction: %s@." (value_to_string (Env.find programROM env));
	Format.printf "  PC: %s@." (value_to_string (Env.find pc env));
  with Not_found -> Format.printf "Not_found dans print_infos. Cela n'affecte pas l'exécution a priori"
	
let cycle curr_cycle p env =	
  Format.printf "Cycle %d@." curr_cycle;
  let inputs = Shared_memory.get_inputs () in
  let env = List.fold_left (fun env id -> Env.add id (read_input inputs id) env) env p.p_inputs in
  let env = List.fold_left (compute_eq p.p_vars) env p.p_eqs in
  List.iter (write_eq env) p.p_eqs; 
  (*List.iter (fun id -> print_output id (Env.find id env)) p.p_outputs;*)
  print_infos env;
  if !wait_between_cycles then
    (
      Format.printf "End Cycle %d   (enter to move on)%!" curr_cycle;
      ignore (read_line())
    );
  env

	
	
let simulate p =
  Init_memory.main reg_tbl rom_tbl ram_tbl p.p_eqs;
  let env = Env.empty in
  let rec cycles curr_cycle env =
	let env = cycle curr_cycle p env in
    cycles (curr_cycle+1) env
  in
  cycles 0 env





















