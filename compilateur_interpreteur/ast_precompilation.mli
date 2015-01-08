type instruction =
  | PIcbeq of int*int
  | PIj of string
  | PIli of int*int
  | PIincr of int*int
  | PImodf of int*int
  | PIlbi of int
  | PIlin of int*int
  | PIso of int*int
  | PIsd of int*int
  | PIcbeqi of int*int

type ligne = (string option)*instruction*int (*label, position*)
(*/!\ position = numero de ligne dans le code initial, pas numero de l'instruction à cause des pseudoinstructions, à utiliser pour les messages d'erreur*)

type programme = ligne list