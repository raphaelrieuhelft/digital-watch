open Ast

let cree_tableau_inst a =
    Array.of_list (List.map fst programme)


let regs = Array.make 32 0

let inv_tbl = Array.map (fun (s,t) -> t) Lexer.reg_tbl

let affiche regs = Array.iter (fun x -> print_string (inv_tbl.(i) ^" = ") ; print_int x ; print string ", ") regs ; print_newline ()
let afficher reste = Array.iter (fun x -> print_int x ; print string ", ") reste

let nimm = Array.make 8 1

let ramaff = Array.make 16 0

let traite t =
    let n = Array.length t in
    let rec aux i =
        affiche regs ;
        afficher nimm ;
        affichier ramaff ;
        print_newline() ;
        if i < n then begin match t.(i) with
            | Ivide -> aux (i+1)
            | Icbeq(x,y) -> if regs.(x) = regs.(y) then aux (i+2) else aux(i+1)
            | Ij x -> aux t.(x)
            | Ili (x,y) -> t.(x) <- y ; aux (i+1)
            | Iincr (x,y) -> t.(x) <- t.(y) + 1 ; aux(i+1)
            | Imodf (x,y) -> t.(x) <- t.(y) mod 4 ; aux(i+1)
            | Ilbi (y)  -> t.(31) <- y ; aux(i+1)
            | Ilin(x,y) -> t.(x) <- nimm.(y)
            | Iso(x,y) -> ramaff.(y) <- t.(x)
            | Isd(x,y) -> ramaff.(y) <- t.(x)
        end;

    in aux 0
