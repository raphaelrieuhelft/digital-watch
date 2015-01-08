exception Cycle
type mark = NotVisited | InProgress | Visited

type 'a graph =
    { mutable g_nodes : 'a node list }
and 'a node = {
  n_label : 'a;
  mutable n_mark : mark;
  mutable n_link_to : 'a node list;
  mutable n_linked_by : 'a node list;
}

let mk_graph () = { g_nodes = [] }

let add_node g x =
  let n = { n_label = x; n_mark = NotVisited; n_link_to = []; n_linked_by = [] } in
  g.g_nodes <- n::g.g_nodes

let node_for_label g x =
  List.find (fun n -> n.n_label = x) g.g_nodes

let add_edge g id1 id2 =
  let n1 = node_for_label g id1 in
  let n2 = node_for_label g id2 in
  n1.n_link_to <- n2::n1.n_link_to;
  n2.n_linked_by <- n1::n2.n_linked_by

let clear_marks g =
  List.iter (fun n -> n.n_mark <- NotVisited) g.g_nodes

let find_roots g =
  List.filter (fun n -> n.n_linked_by = []) g.g_nodes

let has_cycle g =
  let rec visit n =
    match n.n_mark with
      |NotVisited -> 
	n.n_mark<-InProgress;
	List.iter visit n.n_link_to;
	n.n_mark<-Visited
      |InProgress ->
	raise Cycle
      |Visited -> ()
  in
  clear_marks g;
  try List.iter visit g.g_nodes; false with Cycle ->true
  

let topological g =
  let rec find_new  l = 
    match l with
      |[]->[]
      |h::t ->
	h.n_mark<-InProgress; h::(find_new t)
  in 
  if has_cycle g then raise Cycle 
  else begin
    clear_marks g;
    let rec step curr_nodes =
      if curr_nodes=[] then [] else 
      let new_nodes = List.fold_right 
	(fun n l -> (find_new n.n_link_to)@l) curr_nodes [] in
      curr_nodes@(step new_nodes)
    in
    let l =  step (find_roots g) in
    let delete_multiples l = 
      let rec aux l =
	match l with
	  |[]->[]
	  |h::t when h.n_mark=Visited->aux t
	  |h::t -> h.n_mark<-Visited; h::(aux t)
      in
      List.rev (aux (List.rev l))
    in
    List.map (fun n -> n.n_label) (delete_multiples l)
  end




