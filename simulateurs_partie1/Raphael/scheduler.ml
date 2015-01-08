open Netlist_ast
open Graph
exception Combinational_cycle


let read_exp eq = 
  let rec args l = match l with
    |[]->[]
    |(Avar x)::t->x::(args t)
    |_::t->args t
  in
  match (snd eq) with
    |Earg a -> (args [a],[])
    |Ereg id -> [],[id]
    |Enot a -> (args [a],[]) 
    |Ebinop (_,a1,a2) -> (args [a1;a2],[])
    |Emux (a1,a2,a3)-> (args [a1;a2;a3],[])
    |Eram (_,_,ra,_,_,_) ->(args [ra],[]) (*RAMs are updated last*)
    |Erom (_,_,a) ->(args [a],[])
    |Econcat (a1,a2)-> (args[a1;a2],[])
    |Eslice(_,_,a)->(args[a],[])
    |Eselect(_,a)->(args[a],[])


let schedule p = 
  
  (*detect register cycles*)
  
  let reg_cycle eqs vars id= 
    let is_reg id = 
      try (
      let exp = List.assoc id eqs in
      match exp with
	|Ereg id2 -> (true, id2)
	|_-> (false, id))
      with Not_found -> (false, id) (*id is an input*)
    in
    let rec break_cycle id eqs= 
      match List.assoc id eqs with
	|Ereg id2 ->
	  let eqs = (id, Earg (Aconst (VBit
					 false)))::(List.remove_assoc id eqs)  in
	  break_cycle id2 eqs
	|_->eqs
    in
    let rec aux id_start id  = 
      match is_reg id with
	|(true, id2) when id2=id_start->	  
	  break_cycle id_start eqs
	|(true, id2)->aux id_start id2
	|(false, _)-> eqs
    in
    aux id id
  in
  let eqs = Env.fold (fun id _ eqs -> reg_cycle eqs p.p_vars id) p.p_vars p.p_eqs in
  let p = {p_eqs = eqs; p_inputs = p.p_inputs; p_outputs =
      p.p_outputs; p_vars = p.p_vars} in
  
  (*create graph and nodes*)

  let g = mk_graph () in
  List.iter (fun id -> add_node g id) (List.map fst (Env.bindings p.p_vars));
  
  (*add edges*)
  
  let handle_equation eq =
    let l1,l2 = read_exp eq in
    let id=fst eq in
    List.iter (fun id2 -> add_edge g id2 id) l1;
    List.iter (fun id2 -> add_edge g id id2) l2
    
  
  in
  List.iter handle_equation p.p_eqs;
  (*topological sort*)
  try
    (let l = topological g in
         
     let rec build_eqs l = 
       match l with
	 |[]->[]
	 |h::t when (List.mem_assoc h p.p_eqs) ->
	   (h, List.assoc h p.p_eqs)::(build_eqs t)
	 |_::t -> build_eqs t
     in
     let l = build_eqs l in
     {p_eqs=l; p_inputs=p.p_inputs; p_outputs = p.p_outputs;
      p_vars=p.p_vars}
    )
  with Cycle -> raise Combinational_cycle
