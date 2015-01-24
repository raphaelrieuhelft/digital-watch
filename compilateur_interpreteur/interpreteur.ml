open Ast

let cree_tableau_inst a =
    Array.of_list (List.map fst a)


let regs = Array.make 32 0

let inv_tbl = Array.of_list (List.map (fun (s,t) -> s) Lexer.reg_tbl)

let affiche regs = let n= Array.length regs in
    for i =0 to n-1 do
        print_string (inv_tbl.(i) ^" = ") ; print_int regs.(i) ; 
        if i < n-1 then print_string ", " ;

    done;
    print_newline ()

let afficher reste = Array.iter (fun x -> print_int x ; print_string ", ") reste

let nimm = Array.make 8 1

let ramaff = Array.make 16 0

let traite t =
    let n = Array.length t in
    let rec aux i =
        affiche regs ;
        afficher nimm ;
        print_newline();
        afficher ramaff ;
        print_newline() ;
        if i < n then begin match t.(i) with
            | Ivide -> aux (i+1)
            | Icbeq(x,y) -> if regs.(x) = regs.(y) then aux (i+2) else aux(i+1)
            | Ij x -> aux x
            | Ili (x,y) -> regs.(x) <- y ; aux (i+1)
            | Iincr (x,y) -> regs.(x) <- regs.(y) + 1 ; aux(i+1)
            | Imodf (x,y) -> regs.(x) <- regs.(y) mod 4 ; aux(i+1)
            | Ilbi (y)  -> regs.(31) <- y ; aux(i+1)
            | Ilin(x,y) -> regs.(x) <- nimm.(y)
            | Iso(x,y) -> ramaff.(y) <- regs.(x)
            | Isd(x,y) -> ramaff.(y) <- regs.(x)
        end;

    in aux 0
