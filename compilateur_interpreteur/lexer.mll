
(* Analyseur lexical pour mini-C++ *)

{
  open Lexing
  open Parser
   
  exception Lexing_error of string

  (* tables des mots-clés *)
  let kwd_tbl = 
      ["cbeq", CBEQ ; "j", J ;
      "li", LI ; "incr", INCR ; "modf", MODF ; "lbi", LBI ;
      "lin", LIN ; "so", SO; "sd", SD ;
      "cbeqi", CBEQI
    ]

let reg_tbl =
    [ "$zero", 0 ;
    "$at", 1 ;
    "$qt", 2 ;
    "$sc0", 3 ;
    "$sc1", 4;
    "$mn0", 5 ;
    "$mn1", 6  ;
    "$hr0", 7  ;
    "$hr1", 8  ;
    "$da0", 9  ;
    "$da1", 10 ;
    "$da" , 11 ;
    "$mo0", 12 ;
    "$mo1", 13 ;
    "$mo" , 14 ;
    "$yr0", 15 ;
    "$yr1", 16 ;
    "$yr" , 17 ;
    "$nda", 18 ;
	"$in0", 19 ;
    "$in1", 20 ;
    "$in2", 21 ;
    "$in3", 22 ;
    "$in4", 23 ;
    "$in5", 24 ;
    "$t0" , 25 ;
    "$t1" , 26 ;
    "$t2" , 27 ;
    "$s0" , 28 ;
    "$s1" , 29 ;
    "$cr" , 30 ;
    "$k0" , 31 

      ]

  let id_or_kwd = 
    let h = Hashtbl.create 100 in
    List.iter (fun (s,t) -> Hashtbl.add h s t) kwd_tbl;
    let hreg = Hashtbl.create 80 in
    List.iter (fun (s,t) -> Hashtbl.add hreg s t) reg_tbl ;
    fun s -> 
      try Hashtbl.find h s with _ -> (try REG (Hashtbl.find hreg s)  with _ -> ( LABEL s(*raise (Lexing_error "Identifiant inconnu")*)))

  let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- 
      { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }

}


let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let ident = "$"? ( letter | '_' ) ( letter | digit | '_' )*
(*let rident = "$" ident*)
let integer = '0' | ['1'-'9'] digit*
let space = [' ' '\t']
let char = [' '-'!' '#'-'[' ']'-'\127'] | "\\" | "\"" | '\n' | '\t'
let string = '"' char* '"'

rule token = parse
  | '\n'    { newline lexbuf; ENDL (*token lexbuf*) } (* il faut en tenir compte *)
  | space+  { token lexbuf }
  | ident as id { id_or_kwd id }
(*  | rident as id {id_or_kwd id} *)
  | ':'     { COLON }
  | integer as s { INTEGER (int_of_string s) }
  | "//"    { commentendl lexbuf}
  | eof     { EOF }
  | _ as c  { raise (Lexing_error ("illegal character: " ^ String.make 1 c)) }

and commentendl = parse
  | '\n' { newline lexbuf ; ENDL }
  | _  {commentendl lexbuf}
  | eof {EOF}

