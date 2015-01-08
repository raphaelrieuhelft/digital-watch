type instruction = 
  | Icbeq of int*int
  | Ij of int
  | Ili of int*int
  | Iincr of int*int
  | Imodf of int*int
  | Ilbi of int
  | Ilin of int*int
  | Iso of int*int
  | Isd of int*int
  
type programme = (instruction*int) list
(*/!\ int : numero de ligne dans le code initial, pas numero de l'instruction à cause des pseudoinstructions, à utiliser pour les messages d'erreur*)