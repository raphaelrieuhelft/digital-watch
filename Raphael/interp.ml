open Netlist_ast
let n_steps = ref 0
let max_steps = ref (-1)

let (mems : (ident,(value array)) Hashtbl.t) = Hashtbl.create 17


let rec expt n = (*2^n*)
  if n = 0 then 1
  else let r = expt (n/2) in
  if n mod 2 = 0 then r*r  else 2*r*r

let string_of_value v  = 
  let aux b = if b then '1' else '0' in
  match v with
    |VBit b -> String.make 1 (aux b)
    |VBitArray t -> 
      let n = Array.length t in
      let s = String.make n 'a' in
      for i = 0 to n-1 do
	s.[i]<- aux t.(i)
      done;
      s

 let string_is_binary s = 
    let ok = ref true in
    for i = 0 to (String.length s) -1 do
      if s.[i]<>'0'&&s.[i]<>'1' then ok:=false
    done;
    !ok
 
let read_inputs (inputs : ident list) (typs : ty Env.t) =
  Format.printf "Step %d:@." !n_steps;
  let rec read_input id =
    Format.printf "%s ? @?" id;
    let s = read_line () in
    match (Env.find id typs) with
      |TBit -> 
	if (String.length s <> 1 || not (string_is_binary s))
	then 
	  begin
	    Format.printf "Bad input format : one bit expected.@.";
	    read_input id
	  end
	else VBit (s.[0]='1')
      |TBitArray n ->
	if (String.length s <> n || not (string_is_binary s)) 
	then
	  begin
	    Format.printf "Bad input format : %d bits expected (no spaces).@." n;
	    read_input id
	  end
	else 
	  let t = Array.make n false in
	  for i = 0 to n-1 do
	    if s.[i]='1' then t.(i)<-true;
	  done;
	  VBitArray t
  in
  List.map (fun id -> (id, read_input id)) inputs

let print_outputs (outputs : (ident* value) list) = 
  List.iter (fun (id, v) -> Format.printf "=> %s = %s@." id
    (string_of_value v)) outputs


let value_of_arg env = function
  |Avar id -> Env.find id env
  |Aconst v -> v

let bool_of_value = function
  |VBit b -> b
  |VBitArray t -> 
    if Array.length t <> 1 then failwith "Bad typing." else t.(0)

let bool_of_arg env arg = bool_of_value (value_of_arg env arg)

let int_of_value = function
  |VBit _ -> failwith "Bad typing."
  |VBitArray t ->
    let l = Array.to_list t in
    let rec aux l = match l with
      |[]-> 0
      |h::t -> (if h then 1 else 0) + 2*(aux t)
    in
    aux (List.rev l)

let int_of_arg env arg = int_of_value (value_of_arg env arg)

let eval_binop op b1 b2 = 
  match op with
    |Or -> b1||b2
    |Xor -> ( b1 || b2) && (not (b1 && b2))
    |And -> b1&&b2
    |Nand -> not (b1&&b2)

let handle_eq (env : value Env.t ) (vars: ty Env.t) (id,exp) = 
  
  (* compute next value*)
  let v = match exp with
    |Earg arg -> value_of_arg env arg
    |Ereg id2 -> Env.find id2 env
    |Enot arg -> VBit (not (bool_of_arg env arg))
    |Ebinop (op, a1, a2) ->
      let b = eval_binop op (bool_of_arg env a1) (bool_of_arg env a2)
		in
      VBit b
     
    |Emux (a1,a2,a3) ->
       if (bool_of_arg env a1)
	then value_of_arg env a3 
	else value_of_arg env a2 
    |Erom (a_size, w_size, ra) ->
      (Hashtbl.find mems id).(int_of_arg env ra) 
    |Eram(a_size, w_size, ra, we, wa, data) ->
      (Hashtbl.find mems id).(int_of_arg env ra)
    |Econcat (a1, a2) ->
      let v1 = value_of_arg env a1 and v2 = value_of_arg env a2 in
      begin
      match v1, v2 with 
	|VBitArray t1, VBitArray t2 ->
	  VBitArray (Array.append t1 t2)
	|VBit b, VBitArray t ->
	  VBitArray (Array.of_list (b::(Array.to_list t)))
	|VBit b1, VBit b2 ->
	  VBitArray [|b1; b2|] (*autorisé ?*)
	|VBitArray t, VBit b ->
	  VBitArray (Array.of_list ((Array.to_list t)@[b])) (*autorisé ?*)
      end
    |Eslice (i1, i2, arg) ->
      begin
      match value_of_arg env arg with
	|VBitArray t -> VBitArray(Array.sub t i1 (i2 - i1 + 1))
	|_-> failwith "Bad typing."
      end
    |Eselect (i, arg) ->
      match value_of_arg env arg with
	|VBitArray t -> VBit (t.(i))
	|_-> failwith "Bad typing."	  
  in

  (*checks typing and makes required conversions*)
  let check_and_convert_value v id vars = 
    match (v, Env.find id vars) with
      |VBit _, TBit -> v
      |VBit b, TBitArray n when n=1 -> VBitArray [|b|] 
      |VBitArray t, TBit when Array.length t = 1 -> VBit (t.(0))
      |VBitArray t, TBitArray n when Array.length t = n-> v
      |_->failwith "Bad typing : conversion impossible."
  in
  let v = check_and_convert_value v id vars in
  Env.add id v env



let rec step p (env : value Env.t) (inputs : (ident* value) list)
    (vars : ty Env.t) =
  
  (*Process inputs*)
  let env =  List.fold_left (fun env (id, value) -> Env.add id value
    env) env inputs in
  
  (*Process equations*)
  let env = List.fold_left
    (fun e eq -> handle_eq e vars eq) env p.p_eqs in
  
  (*Update RAMs*)
  
  List.iter 
    (fun (id,exp)->match exp with
      |Eram(a_size, w_size, ra, we, wa, data)
	-> if bool_of_arg env we 
	  then (Hashtbl.find mems id).(int_of_arg env wa)<-(value_of_arg
							  env data)
      |_->()
    ) p.p_eqs;
  
  (*Print outputs and go to next step*)
  incr n_steps;
  print_outputs (List.map (fun id -> (id, Env.find id env)) p.p_outputs);
  if (!n_steps=(!max_steps)) then ()
  else step p env (read_inputs p.p_inputs p.p_vars) vars

let init p =
  (*Initialize RAMs/ROMs*)
  let init_mem a_size w_size =
    let n = expt a_size in
    let wl = w_size in
    let t = Array.make n (VBitArray [||]) in
    for i = 0 to n-1 do
      t.(i)<-VBitArray (Array.make wl false)
    done;
    t
  in
   List.iter 
    (fun (id,exp) -> match exp with
      |Eram (a_size, w_size, _,_,_,_) ->
	Hashtbl.add mems id (init_mem a_size w_size)
      |Erom (a_size, w_size, _) ->
	let a_n = expt a_size in
	let w_l = w_size in
	let t = init_mem a_size w_size in
	Format.printf "Initializing ROM %s (%d words of length %d).@."
	  id a_n w_l;
	for i = 0 to a_n -1 do
	  let rec aux () = 
	    Format.printf "Word %d ? @?" i;
	    let s = read_line () in
	    if (not (string_is_binary s) || String.length s <> w_l)
	    then 
	      begin
		Format.printf "Bad input, %d bits expected.@." w_l;
		aux ()
	      end
	    else 
	      let u = Array.make w_l false in
	      for j = 0 to w_l - 1 do
		if s.[j] = '1' then u.(j)<-true;
	      done;
	      t.(i)<-VBitArray u;
	  in
	  aux () 
	done;
	Hashtbl.add mems id t
      |_->()
    ) p.p_eqs;

  (*Initialize registers*)
  let env = List.fold_left 
    (fun env (id,exp) -> match exp with
      |Ereg id2 -> Env.add id2 (VBit false) env
      |_-> env)
    Env.empty p.p_eqs in
  (*Launch simulation*)
  step p env  (read_inputs p.p_inputs p.p_vars) p.p_vars
