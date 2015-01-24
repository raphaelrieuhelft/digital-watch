open Ast

let rec convertit n lim = match lim with
    | 0 -> if n > 0 then failwith "Dépassement taille, n est trop grand"
            else []
    | lim -> (n mod 2)::(convertit (n/2) (lim -1))


let conv_s l =
    String.concat "" (List.map string_of_int l)


let cvs n lim =  conv_s (convertit n lim)

let convrs n = cvs n 5

let rec prod_inst = function
    | Ivide -> "" (* faire gaffe si pas d'interaction malencontresue avec le code precompilateur de Diane *)
    | Icbeq (x,y) -> "0110" ^ (convrs x) ^(convrs y) ^ "00"
    | Ij x -> "0100" ^ (cvs x 9) ^ "000"
    | Ili (x,y) -> "1001" ^ (convrs x) ^(cvs y 7)
    | Iincr (x,y) -> "1110" ^ (convrs x) ^(convrs y) ^ "00"
    | Imodf(x,y) -> "1100" ^ (convrs x) ^(convrs y) ^ "00"
    | Ilbi x -> "1000" ^ (cvs x 12)
    | Ilin(x,y) -> "1010" ^ (convrs x) ^(cvs x 3) ^ "0000"
    | Iso(x,y) -> "0000"  ^ (convrs x) ^ (cvs x 4) ^ "000"
    | Isd(x,y) ->"0010" ^ (convrs x) ^(cvs x 4) ^ "000"


let prod_prog  p = String.concat "\n" (List.map (fun (x,y) -> prod_inst x) p)
