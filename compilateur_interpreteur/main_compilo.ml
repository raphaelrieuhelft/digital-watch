
(* Fichier principal du compilateur mini-c++ *)

open Format
open Lexing
(* Option de compilation, pour s'arrêter à l'issue du parser *)
let parse_only = ref false
let type_only = ref false

(* Noms des fichiers source et cible *)
let ifile = ref ""
let ofile = ref ""

let set_file f s = f := s 

(* Les options du compilateur que l'on affiche en tapant arithc --help *)
let options = 
  ["--parse-only", Arg.Set parse_only, 
   "  Pour ne faire uniquement que la phase d'analyse syntaxique" ; 
   "--type-only", Arg.Set type_only ,
   "  Pour ne faire que les phases d'analyse syntaxique et de typage" ;
   "-o", Arg.String (set_file ofile), 
   "<file>  Pour indiquer le mom du fichier de sortie"]

let usage = "usage: minic++ [option] file.cpp"

(* localise une erreur en indiquant la ligne et la colonne *)
let localisation pos =
  let l = pos.pos_lnum in
  let c = pos.pos_cnum - pos.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" !ifile l (c-1) c

let doublelocal (p,q) =
  let l1 = p.pos_lnum in
  let c1 = p.pos_cnum - p.pos_bol + 1 in
  let l2 = q.pos_lnum in
  let c2 = q.pos_cnum - q.pos_bol + 1 in
	if l1 = l2 then
		eprintf "L'erreur se trouve à la ligne %d, commence au caractère %d et se finit au caractère %d :\n" l1 c1 c2	
	else eprintf "L'erreur se trouve entre les lignes %d et %d, commence au caractère %d et se finit au caractère %d :\n" l1 l2 c1 c2
let () = 
  (* Parsing de la ligne de commande *)
  Arg.parse options (set_file ifile) usage;

  (* On vérifie que le nom du fichier source a bien été indiqué *)
  if !ifile="" then begin eprintf "Aucun fichier à compiler\n@?"; exit 1 end; 

  (* Ce fichier doit avoir l'extension .cpp *)
  if not (Filename.check_suffix !ifile ".cpp") then begin
    eprintf "Le fichier d'entrée doit avoir l'extension .cpp\n@?";
    Arg.usage options usage;
    exit 1
  end;

  if !ofile="" then ofile := Filename.chop_suffix !ifile ".exp" ^ ".s";
  (* Ouverture du fichier source en lecture *)
  let f = open_in !ifile in
    
  (* Création d'un tampon d'analyse lexicale *)
  let buf = Lexing.from_channel f in
  
  try
    (* Parsing: la fonction  Parser.prog transforme le tampon lexical en un 
       arbre de syntaxe abstraite si aucune erreur (lexicale ou syntaxique) 
       n'est détectée.
       La fonction Lexer.token est utilisée par Parser.prog pour obtenir 
       le prochain token. *)
    let p = Parser.fichier Lexer.token buf in
	close_in f ;
       	(* On s'arrête ici si on ne veut faire que le parsing *)
   	if !parse_only then 
		exit 0
   	else let tarbre = Typing.typfichier p in
   			if !type_only then
				(*print_string "Typage correct\n";*)
				exit 0
			else begin
				Compilateur.compile_fichier tarbre !ofile ;
				(*print_string "OK.\n";*)
				exit 0;
			end
       	(*Interp.prog p *)
  with
    | Lexer.Lexing_error c -> 
	(* Erreur lexicale. On récupère sa position absolue et 
	   on la convertit en numéro de ligne *)
	localisation (Lexing.lexeme_start_p buf);
	eprintf "Erreur dans l'analyse lexicale: %s@." c;
	exit 1
    | Parser.Error -> 
	(* Erreur syntaxique. On récupère sa position absolue et on la 
	   convertit en numéro de ligne *)
	localisation (Lexing.lexeme_start_p buf);
	eprintf "Erreur dans l'analyse syntaxique@.";
	exit 1
    | Typing.Error (loc, s) -> 
	(* Erreur syntaxique. On récupère sa position absolue et on la 
	   convertit en numéro de ligne *)
	localisation (fst loc) ;
	(*localisation (snd loc) ;*)
	doublelocal loc ;
	eprintf "Erreur dans le typage: %s@." s;
	exit 1
    | Typing.Not_implementedt s | Compilateur.Not_implementedc s ->
	localisation (Lexing.lexeme_start_p buf);
	eprintf "Erreur du compilateur (caractéristique non implémentée) :  %s" s;
	exit 3
    | Typing.Class_not_found s -> localisation (Lexing.lexeme_start_p buf);
        eprintf "Erreur du compilateur (provisoire) :  %s" s;
        exit 2
    | Failure s ->
	localisation (Lexing.lexeme_start_p buf);
        eprintf "Erreur du compilateur : message :  %s" s;
	exit 2;
   | _ -> 
        localisation (Lexing.lexeme_start_p buf);
        eprintf "Erreur du compilateur.\n";
        exit 2 
	




