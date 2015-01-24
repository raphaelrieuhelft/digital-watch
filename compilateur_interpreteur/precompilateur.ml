open Ast_precompilation
open Ast

exception LabelDejaExistant of (string*int)
exception LabelInexistant of (string*int)
exception ConflitLabels of int

module Smap = Map.Make(String)

let reg_at = 1 (*?*)

(* Si pas traité avant, on numérote les lignes *)
let rec numerote_lignes pos = function
  | [] -> []
  | (so, inst, _)::l -> (so, inst, pos)::(numerote_lignes (pos+1) l)
  
let max_string_options pos = function
  | None, so -> so
  | so, None -> so
  | _ -> raise (ConflitLabels pos)

let rec handle_cbeqi_and_empty = function
  | [] -> []
  | (so, PIcbeqi(rs,imm), pos)::tl ->
    (so, PIli(reg_at,imm), pos)::(None, PIcbeq(rs,reg_at), pos)::(handle_cbeqi_and_empty tl)
  | (so1, PIvide, pos1)::(so2, pi, pos2)::tl ->
    let so = max_string_options pos1 (so1,so2) in
	handle_cbeqi_and_empty ((so, pi, pos2)::tl)
  | hd::tl -> hd::(handle_cbeqi_and_empty tl)

let make_labels p =
  (*let n = List.length p in*)
  List.fold_left (fun (i,smap) (so,_,pos) ->
    match so with
	  | None -> (i+1, smap)
	  | Some lab ->
	    if Smap.mem lab smap then raise (LabelDejaExistant (lab,pos));
		(i+1, Smap.add lab i smap)
  ) (0, Smap.empty) p
  
let handle_labels labels = List.map (fun (_,instr,pos) ->
  match instr with
    | PIvide ->  assert false (*(Ast.Ivide, pos)*)
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
	| PIcbeqi(a,b) -> failwith "Cela doit déjà être retiré (PIcbeqi) "
)

let main p =
try
  (*let p_num = numerote_lignes 1 p in*)
  let p2 = handle_cbeqi_and_empty p in
  let i, labels = make_labels p2 in
  handle_labels labels p2
with
  | LabelDejaExistant(lab,pos) -> 
    Format.eprintf "Ligne %d : définition d'un label \"%s\" déjà existant.@." pos lab;
	exit 2
  | LabelInexistant(lab,pos) -> 
    Format.eprintf "Ligne %d : label \"%s\" inconnu.@." pos lab;
	exit 2
  | ConflitLabels(pos) -> 
    Format.eprintf "Ligne %d : deux labels sont définis successivement à cause d'une instruction vide ; on ne sait pas retenir les deux@." pos;
	exit 2
  
