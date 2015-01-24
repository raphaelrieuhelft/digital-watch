open Ast_precompilation
open Ast

exception LabelDejaExistant of (string*int)
exception LabelInexistant of (string*int)

module Smap = Map.Make(String)

let reg_at = 1 (*?*)

(* Si pas traité avant, on numérote les lignes *)
let rec numerote_lignes pos = function
  | [] -> []
  | (so, inst, _)::l -> (so, inst, pos)::(numerote_lignes (pos+1) l)

let rec handle_cbeqi = function
  | [] -> []
  | (so, PIcbeqi(rs,imm), pos)::tl ->
    (so, PIli(reg_at,imm), pos)::(None, PIcbeq(rs,reg_at), pos)::(handle_cbeqi tl)
  | hd::tl -> hd::(handle_cbeqi tl)

let make_labels p =
  let n = List.length p in
  List.fold_left (fun (i,smap) (so,_,pos) ->
    match so with
	  | None -> smap
	  | Some lab ->
	    if Smap.mem lab smap then raise (LabelDejaExistant (lab,pos));
		Smap.add lab i smap
  ) (0, Smap.empty) p
  
let handle_labels labels = List.map (fun (_,instr,pos) ->
  match instr with
    | PIvide -> Ivide
    | PIj lab -> begin try (Ij (Smap.find lab labels), pos)
	  with Not_found -> raise (LabelInexistant (lab,pos)) end
	| PIcbeq(a,b) -> (Icbeq(a,b), pos)
	| PIli(a,b) -> (Ili(a,b), pos)
    | PIincr(a,b) -> (Iincr(a,b), pos)
    | PImodf(a,b) -> (Imodf(a,b), pos)
    | PIlbi a -> (Ilbi a, pos)
    | PIlin(a,b) -> (Ilin(a,b), pos)
    | PIso(a,b) -> (Iso(a,b), pos)
    | PIsd(a,b) -> (Isd(a,b), pos)
)

let main p =
  let p_num = numerote_lignes 0 p in
  let p2 = handle_cbeqi p_num in
  let labels = make_labels p2 in
  handle_labels labels p2
