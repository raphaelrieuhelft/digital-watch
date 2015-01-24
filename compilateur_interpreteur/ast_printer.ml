open Ast

let print_inst i = match (fst i) with
  |Icbeq (i,j) -> Format.printf "cbeq %d %d@." i j
  |Ij i -> Format.printf "j %d@." i
  |Ili (i,j) -> Format.printf "li %d %d@." i j
  |Iincr (i,j) ->Format.printf "incr %d %d@." i j
  |Imodf (i,j) -> Format.printf "incr %d %d@." i j
  |Ilbi i -> Format.printf "lbi %d@." i
  |Ilin (i,j) -> Format.printf "lin %d %d@." i j
  |Iso (i,j) -> Format.printf "so %d %d@." i j
  |Isd (i,j) -> Format.printf "sd %d %d@." i j

let print_program p = List.iter print_inst p
  
