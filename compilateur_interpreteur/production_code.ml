open Ast

exception Depassement of string * int
exception LDepassement of string * int * int

let rec convertit n lim = match lim with
    | 0 -> if n > 0 then raise (Depassement ("Dépassement taille, n est trop grand",n))
            else []
    | lim -> (n mod 2)::(convertit (n/2) (lim -1))

let convertit2 n lim = try convertit n lim with Depassement (s, i) -> raise (Depassement (s,n))

let conv_s l =
    String.concat "" (List.map string_of_int l)


let cvs n lim =  conv_s (convertit2 n lim)

let convrs n = cvs n 5

let rec prod_inst = function
    | Icbeq (x,y) -> "0110" ^ (convrs x) ^(convrs y) ^ "00"
    | Ij x -> "0100" ^ (cvs x 9) ^ "000"
    | Ili (x,y) -> "1001" ^ (convrs x) ^(cvs y 7)
    | Iincr (x,y) -> "1110" ^ (convrs x) ^(convrs y) ^ "00"
    | Imodf(x,y) -> "1100" ^ (convrs x) ^(convrs y) ^ "00"
    | Ilbi x -> "1000" ^ (cvs x 12)
    | Ilin(x,y) -> "1010" ^ (convrs x) ^(cvs y 3) ^ "0000"
    | Iso(x,y) -> "0000"  ^ (convrs x) ^ (cvs y 4) ^ "000"
    | Isd(x,y) ->"0010" ^ (convrs x) ^(cvs y 4) ^ "000"


let prod_prog  p = (*String.concat "\n" *)(List.map (fun (x,y) -> try prod_inst x with Depassement (s,i) -> raise (LDepassement (s^"\nA la ligne : "^string_of_int(y),i, y))) p)
