open Format
open Lexing
open Globals

(* localise une erreur en indiquant la ligne et la colonne *)
let localisation pos =
  let l = pos.pos_lnum in
  let c = pos.pos_cnum - pos.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" !compiler_source_filename l (c-1) c

let doublelocal (p,q) =
  let l1 = p.pos_lnum in
  let c1 = p.pos_cnum - p.pos_bol + 1 in
  let l2 = q.pos_lnum in
  let c2 = q.pos_cnum - q.pos_bol + 1 in
	if l1 = l2 then
		eprintf "L'erreur se trouve � la ligne %d, commence au caract�re %d et se finit au caract�re %d :\n" l1 c1 c2	
	else eprintf "L'erreur se trouve entre les lignes %d et %d, commence au caract�re %d et se finit au caract�re %d :\n" l1 l2 c1 c2
	
	
let precompile () = 
  let f = open_in !compiler_source_filename in
  let buf = Lexing.from_channel f in
  
  try
    (* Parsing: la fonction  Parser.prog transforme le tampon lexical en un 
       arbre de syntaxe abstraite si aucune erreur (lexicale ou syntaxique) 
       n'est d�tect�e.
       La fonction Lexer.token est utilis�e par Parser.prog pour obtenir 
       le prochain token. *)
    let p = Parser.prog Lexer.token buf in
    close_in f;  
    let p2 = Precompilateur.main p in
    if !print_precompiled then
      begin
	let out = open_out !compiler_printprogram_filename in
	let ff = Format.formatter_of_out_channel out in
	Ast_printer.print_program ff p2
      end;
    p2
  with
    | Lexer.Lexing_error c -> 
	localisation (Lexing.lexeme_start_p buf);
	eprintf "Erreur dans l'analyse lexicale: %s@." c;
	exit 2
    | Parser.Error -> 
	localisation (Lexing.lexeme_start_p buf);
	eprintf "Erreur dans l'analyse syntaxique@.";
	exit 2
	| exn -> eprintf "Erreur inconnue dans Compiler.compile@."; raise exn
  (*  | Failure s ->
	localisation (Lexing.lexeme_start_p buf);
        eprintf "Erreur du compilateur : message :  %s" s;
	exit 2;
   | _ -> 
        localisation (Lexing.lexeme_start_p buf);
        eprintf "Erreur du compilateur.\n";
        exit 2 
*)
let compile p =
	let out = open_out !compiler_out_filename in
	let ff = Format.formatter_of_out_channel out in
	List.iter (fun s -> fprintf ff "%s@." s) (Production_code.prod_prog p)
let interp p =
  Interpreteur.traite (Interpreteur.cree_tableau_inst p)
