open Netlist_ast

let random = ref false
exception InvalidValue



let to_bit s = 
    if not(String.length s = 1) then raise InvalidValue;
    let b = match s.[0] with
      | '0' -> false
	  | '1' -> true
	  | _ -> raise InvalidValue
    in VBit b

let to_bool_array n s =
    if not(String.length s = n) then raise InvalidValue;
    let t = Array.make n false in
    for i=0 to n-1 do
      match s.[i] with
      | '0' -> ()
  	  | '1' -> t.(i) <- true
	  | _ -> raise InvalidValue
    done;
    t
let to_bit_array n s = VBitArray (to_bool_array n s)

let to_value_with_ty ty s = match ty with
  | TBit -> to_bit s
  | TBitArray n -> to_bit_array n s 
  
let ty_description = function
  | TBit -> "bit"
  | TBitArray n -> Format.sprintf "%d bit(s)" n

let actual_read_value_with_ty ty =
  let s = read_line () in
  try
    to_value_with_ty ty s
  with InvalidValue ->
    Format.printf "    invalid value : %s expected@." (ty_description ty);
    raise InvalidValue


let random_value_with_ty = function
  | TBit ->
    let b = Random.bool () in
    Format.printf "%s@." (if b then "1" else "0");
    VBit b
  | TBitArray n ->
    let t = Array.make n false in
    for i=0 to n-1 do 
      let b = Random.bool () in
      Format.printf "%s" (if b then "1" else "0");
      t.(i) <- b;
    done;
    Format.printf "@.";
    VBitArray t

let read_value_with_ty ty =
  Format.printf " (%s) ? %!" (ty_description ty);
  if !random then
    random_value_with_ty ty
  else
    actual_read_value_with_ty ty

  
let rec read_entry_with_ty ty id =
  Format.printf "  input %s" id;
  try read_value_with_ty ty
  with InvalidValue -> read_entry_with_ty ty id
let read_entry ty_env id =
  let ty = Env.find id ty_env in
  read_entry_with_ty ty id

let read_bool_array n =
  match read_value_with_ty (TBitArray n) with
    | VBit _ -> assert false
    | VBitArray t -> t
