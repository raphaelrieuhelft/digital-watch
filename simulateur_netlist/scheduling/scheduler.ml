open Netlist_ast
open Graph


exception Combinational_cycle


let read_arg = function
  | Avar id -> [id]
  | Aconst _ -> []
	
(* Netlist_ast .equation -> Netlist_ast .ident list*)
let read_exp eq = 
  match snd eq with
    | Ereg _ -> []
    | Earg arg | Enot arg 
	| Erom (_,_,arg) | Eram (_,_,arg,_,_,_)
	| Eslice (_,_,arg) | Eselect (_,arg)
	  -> read_arg arg
	| Ebinop (_,arg1,arg2)
    | Econcat (arg1,arg2)
      -> (read_arg arg1) @ (read_arg arg2)
	| Emux (arg1,arg2,arg3) -> (read_arg arg1) @ (read_arg arg2) @ (read_arg arg3)



let rec partial_map f = function
  | [] -> []
  | h::t -> 
    let t1 = partial_map f t in
	match f h with
	  | None -> t1
	  | Some h1 -> h1::t1
	
(* Netlist_ast .program -> Netlist_ast .program *)
let schedule p = 
  let g = mk_graph () in
  Env.iter (fun id _ -> add_node g id) p.p_vars;
  List.iter (fun eq ->
    List.iter (fun id -> add_edge g id (fst eq)) (read_exp eq);
  ) p.p_eqs;
  if has_cycle g then raise Combinational_cycle;
  let ids = topological g in
  let eq_opt_of_id id = try Some (List.find (fun eq -> fst eq = id) p.p_eqs) 
    with Not_found -> assert (List.mem id p.p_inputs); None in
  let eqs = partial_map eq_opt_of_id ids in
  { p_eqs = eqs; p_inputs = p.p_inputs; p_outputs = p.p_outputs; p_vars = p.p_vars }
