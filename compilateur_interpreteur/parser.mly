
/* Analyseur syntaxique pour Mini-C++ */

%{
  open Ast_precompilation
%}

%token <int> INTEGER
%token <int> REG
%token <string> LABEL
%token <string> CHAINE
%token ENDL
%token CBEQ J LI INCR MODF LBI LIN SO SD CBEQI
%token EOF



/* Point d'entrée de la grammaire */
%start <Ast_precompilation.programme>prog

/* Type des valeurs retournées par l'analyseur syntaxique */

%%

prog:
    | ENDL * ; x = ligne * { x } ;

ligne:
    l = option(LABEL) ; i = inst ; ENDL ; ENDL *  {(l, i, 0)} ; /* moyen pour ligne ? au pire on rénumérote après */

inst:
    | CBEQ ; x = REG ; y = REG  { PIcbeq (x,y) }
    | J ; s = LABEL { PIj s }
    | LI ; x = REG ; y = INTEGER {PIli (x,y) }
    | 
    |
    |
    |


/*
fichier:
| x = position(dfichier) { x }
;

dfichier:
| y=boption(INCLUDEIOS)  ; x = decl * ; EOF { {bincludeios = y ; decls =  x} }
;

position(X):
| x = X { {v = x ; loc=($startpos, $endpos) }} 


decl:
| x = position(ddecl) { x }

ddecl:
| x = decl_vars { Dv (x) }
| x = decl_class { Dc (x) }
| x = proto ; y = bloc { Db (x,y)}
;

decl_vars:
| x = position(ddecl_vars) { x }
;

ddecl_vars:
| x = typ; y = separated_nonempty_list(COMMA, var) ; SEMICOLON { Declv (x,y) }
;



/* On veut pouvoir déclarer des attributs du type de la classe, il faut donc enregistrer que la classe est un nouveau type. */

debut_decl_class:
| CLASS ; z = IDENT; { Hashtbl.add (Lexerhack.table) z () ; z }

decl_class:
| x = position(ddecl_class) {x}

ddecl_class:
| z = debut_decl_class ; l = optsupers  ; LACC ; PUBLIC; COLON; y = member * ; RACC ; SEMICOLON 
{  Class (z,l,y) }  
;

optsupers:
| x = option(supers) { match x with | None -> { v = (Super []) ; loc=($startpos,$endpos) }
				    | Some a -> a  
		     }
;

supers:
| x = position(dsupers) { x }
;

ddsupers:
| PUBLIC ; s = TIDENT { s }
;

dsupers:
|COLON;  z = separated_nonempty_list(COMMA, ddsupers) { Super z } 
;



member:
| x = position(dmember) { x }
;

dmember:
| x = decl_vars { Mvar (x) }
| VIRTUAL; x = proto ; SEMICOLON { Mmeth (true,x) }
| x = proto ; SEMICOLON { Mmeth (false,x) }
;


proto:
| x = position(dproto) { x }

dproto: x = typ ; y = qvar ; LPAR ; z = separated_list(COMMA, argument) ; RPAR 
 { Proto ( x, y, z) }
 | x = TIDENT ;  LPAR ; z = separated_list(COMMA, argument) ; RPAR 
 { Pcons (x , z)}
 | x = TIDENT  ; COLON ; COLON  ; y = TIDENT ; LPAR ; z = separated_list(COMMA, argument) ; RPAR 
   { Pconshc ( x, y, z) } 
;

typ:
| VOID { Void }
| INT { Int }
| s = TIDENT  { Tid s }
;

argument:
| x = position(dargument) { x }
;

dargument:
| x = typ ; y = var { Arg (x,y) } 
;


var:
| x = position(dvar) { x }
;


dvar:
| x = IDENT { Ident x}
| TIMES ; x = var { (transmet true x).v }
| ADDR ; x = var { (transmet false x).v }
; 

qvar:
| x = position(dqvar) { x }
;

dqvar:
| x = qident { Qvar x }
| TIMES ; x = qvar { (transmetq true x).v }
| ADDR ; x = qvar { (transmetq false x).v  }
; 

qident:
| x = position(dqident) { x }
;

dqident:
| x = IDENT { Qident x }
| x = TIDENT ; COLON ; COLON ; y = IDENT {  Qmeth (x,y) }
;


expr:
| x = position(dexpr) { x }

dexpr:
| x = expr; y = operateur; z = expr {  Eop (y,x,z) }
| ADDR; x = expr { Eaddr x }
| NOT; x = expr { Enot x }
| MINUS; x = expr %prec UNAIRE { Euminus( x) }
| PLUS; x = expr %prec UNAIRE {Euplus( x) }
| TIMES; x = expr %prec UNAIRE { Epointeur (x) }
| INCR; x = expr { Elincr x }
| DECR; x = expr { Eldecr x }
| x = expr; DOT; y = IDENT { Eattr (x,y) }
| x = expr; SDEREF; y = IDENT { Esderef (x,y) }
| x = expr; ASSIGN; y = expr { Eassign (x,y) }
| x = expr; LPAR; y = separated_list(COMMA, expr) ; RPAR { Efcall (x,y) }
| x = expr; INCR { Erincr x }
| x = expr; DECR { Erdecr x }
| x = INTEGER { Eint x }
| THIS { Ethis } 
| FALSE { Ebool false}
| TRUE { Ebool true }
| NULL { Enull }
| x = qident { Eqident x }
| LPAR; x = expr; RPAR { Epar x }
| NEW; x = TIDENT; LPAR; y = separated_list(COMMA, expr) ; RPAR { Enew (x,y) }
;


%inline operateur:
| EQ {Eq}| NEQ {Neq}| LT {Lt} | LE {Le} | GT {Gt} | GE {Ge} 
| PLUS {Add} | MINUS {Sub} | TIMES {Mul} | DIV {Div} | MODULO {Mod} 
| AND {And} | OR {Or}
;


vinst: CHEVRON ; e = expr_str { e }
;

dinst:
| SEMICOLON { Nothing }
| e = expr ; SEMICOLON { Iexpr e }
| t = typ; v = var ; SEMICOLON { Idecl (t,v) }
| t = typ; v = var ; ASSIGN; e = expr SEMICOLON { Ideclinit (t,v,e) }
| t = typ; v = var ; ASSIGN; s = TIDENT ; LPAR; e = separated_list(COMMA, expr) ; RPAR ; SEMICOLON
{ Ideclobj (t,v,s,e) }
| IF ; LPAR ; e = expr ; RPAR ; i = inst %prec IFX { If (e,i) }
| IF ; LPAR ; e = expr ; RPAR ; i = inst ; ELSE ; y = inst 
{Ifelse (e,i,y) }
| WHILE ; LPAR ; e = expr ; RPAR ; i = inst  { While (e,i) }
| FOR LPAR ; e = separated_list (COMMA , expr); SEMICOLON;  x = option(expr)  ; SEMICOLON;
 f = separated_list (COMMA , expr) ; RPAR ; i = inst { match x with
							| None -> Afor (e,f,i)
							| Some ex -> For (e,ex,f,i) }
| b = bloc { Ibloc b }
| STDCOUT ; l = nonempty_list (vinst); SEMICOLON { Cout l }
| RETURN ; ex = option(expr)  ; SEMICOLON  { match ex with
						| None -> Areturn
						| Some e -> Return e }
;


inst:
| x = position(dinst) { x } 
;


expr_str:
| x = position(dexpr_str) { x }
;
dexpr_str:
| x = expr { Esexpr x }
| x = CHAINE { Estring x}
;


bloc:
| x = position(dbloc) { x }
;

dbloc:
  LACC; x = inst * ; RACC { Bloc x }
;


*/
