exception Cycle
type mark = NotVisited | InProgress | Visited

(* 'a graph -> bool *)
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
  clear_marks g;
  let rec check_node n =
    match n.n_mark with
      | Visited -> ()
      | InProgress -> raise Cycle
      | NotVisited ->
	n.n_mark <- InProgress;
	List.iter check_node n.n_link_to;
	n.n_mark <- Visited
  in
  let ans =
    try
      List.iter check_node g.g_nodes;
      false
    with 
      Cycle -> true
  in
  clear_marks g;
  ans

  
(*  'a graph -> 'a list *)
let topological g = 
  clear_marks g;
  let ans = ref [] in
  let next = ref (find_roots g) in
  let is_visited n = match n.n_mark with
    | Visited -> true
    | InProgress | NotVisited -> false
  in
  let handle_node n =
    if (not (is_visited n)) && (List.for_all is_visited n.n_linked_by) then
      (n.n_mark <- Visited; ans := n.n_label :: !ans);
    List.iter (fun n1 -> next := n1 :: !next) n.n_link_to
  in
  let rec handle_curr curr = match curr with
    | [] -> ()
    | h::t -> handle_node h; handle_curr t
  in
  let rec loop () =
    let curr = !next in
    if curr = [] then () 
    else
      begin
      next := [];
      handle_curr curr;
      loop ()
      end
  in
  loop ();
  clear_marks g;
  List.rev !ans
    
  
  

