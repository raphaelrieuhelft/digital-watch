open Ast
open Globals
open Shared_memory

let cree_tableau_inst a =
    Array.of_list a


let regs = Array.make 32 0

let inv_tbl = Array.of_list (List.map (fun (s,t) -> s) Lexer.reg_tbl)

let affiche regs = let n= Array.length regs in
    for i =0 to n-1 do
        print_string (inv_tbl.(i) ^" = ") ; print_int regs.(i) ; 
        if i < n-1 then print_string ", " ;

    done;
    print_newline ()

let afficher reste = Array.iter (fun x -> print_int x ; print_string ", ") reste

let nimm = Array.make 8 0

let dec7 = Read_binary_file.main dec7_addr_size
  dec7_word_size dec7_filename

let rec bools_of_int n i = match i with 
  |0 -> if n>0 then failwith"n trop grand" else []
  |i -> (n mod 2 = 1)::(bools_of_int (n/2) (i-1))

let traite t =
    let n = Array.length t in
   
    let rec aux i =
      Format.eprintf "Ligne dans le code : %d. Numéro d'instruction : %d.@." (snd t.(i)) i;
      ignore(read_line());
      affiche regs ;
      afficher nimm ;
      print_newline();
        (*afficher ramaff ;
        print_newline() ;*)
        if i < n then begin match fst t.(i) with
            | Icbeq(x,y) -> if regs.(x) = regs.(y) then aux (i+2) else aux(i+1)
            | Ij x -> aux x
            | Ili (x,y) -> regs.(x) <- y ; aux (i+1)
            | Iincr (x,y) -> regs.(x) <- regs.(y) + 1 ; aux(i+1)
            | Imodf (x,y) -> regs.(x) <- regs.(y) mod 4 ; aux(i+1)
            | Ilbi (y)  -> regs.(31) <- y ; aux(i+1)
            | Ilin(x,y) -> regs.(x) <- nimm.(y) ; aux (i+1)
            | Iso(x,y) -> write_in_digitsRAM y (Array.of_list
						  (bools_of_int x 7));
              aux (i+1)
            | Isd(x,y) -> write_in_digitsRAM y dec7.(x); aux (i+1)
        end;

    in aux 0
