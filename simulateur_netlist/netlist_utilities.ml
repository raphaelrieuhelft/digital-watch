open Netlist_ast

let bool_of_value = function
  | VBit b -> b
  | VBitArray t when Array.length t = 1 -> t.(0)
  | VBitArray _ -> failwith "bool_of_value"  
let array_of_value = function
  | VBit b -> [|b|]
  | VBitArray t -> t


let zero_of_ty = function
  | TBit -> VBit false
  | TBitArray n -> VBitArray (Array.make n false)
  
let value_to_value_with_ty v ty = match v,ty with
  | VBit _ , TBit -> v
  | VBitArray t , TBitArray n when Array.length t = n -> v
  | VBit b , TBitArray 1 -> VBitArray [|b|]
  | VBitArray t , TBit when Array.length t = 1 -> VBit t.(0)
  | _ -> failwith "value_to_value_with_ty"


let bits_to_int t =
  let rec aux i pow acc =
    if i<0 then acc 
	else
	  let acc = if t.(i) then acc+pow else acc in
	  aux (i-1) (2*pow) acc
  in aux ((Array.length t)-1) 1 0
(*
let bits_to_int = Array.fold_left
  (fun n b -> (2*n) + (if b then 1 else 0)) 0
*)


let value_to_string = function
  | VBit b -> if b then "1" else "0"
  | VBitArray t ->
    let n = Array.length t in
	let s = String.make n '0' in
	for i=0 to n-1 do
	  if t.(i) then s.[i] <- '1'
	done;
	s


  
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








