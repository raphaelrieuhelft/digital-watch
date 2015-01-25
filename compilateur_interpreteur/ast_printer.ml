open Ast

let print_inst ff i = match (fst i) with
  |Icbeq (i,j) -> Format.fprintf ff "cbeq %d %d@." i j
  |Ij i -> Format.fprintf ff "j %d@." i
  |Ili (i,j) -> Format.fprintf ff "li %d %d@." i j
  |Iincr (i,j) ->Format.fprintf ff "incr %d %d@." i j
  |Imodf (i,j) -> Format.fprintf ff "incr %d %d@." i j
  |Ilbi i -> Format.fprintf ff "lbi %d@." i
  |Ilin (i,j) -> Format.fprintf ff "lin %d %d@." i j
  |Iso (i,j) -> Format.fprintf ff "so %d %d@." i j
  |Isd (i,j) -> Format.fprintf ff "sd %d %d@." i j

let print_program ff p = List.iter (print_inst ff) p
  
