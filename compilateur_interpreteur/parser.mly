
/* Analyseur syntaxique  */

%{
  open Ast_precompilation
%}

%token <int> INTEGER
%token <int> REG
%token <string> LABEL
%token COLON
%token ENDL
%token CBEQ J LI INCR MODF LBI LIN SO SD CBEQI
%token EOF



/* Point d'entrée de la grammaire */
%start <Ast_precompilation.programme>prog

/* Type des valeurs retournées par l'analyseur syntaxique */

%%

prog:
    | ENDL * ; x = ligne * ; EOF { x } ;

label:
    | l = LABEL ; COLON {l} ;
ligne:
    | l = label ; ENDL {(Some(l), PIvide, 0)}
    | l = option(label) ; i = inst ; ENDL ; ENDL *  {(l, i, 0)} ; /* moyen pour ligne ? au pire on rénumérote après */

int_ou_reg:
    | x = REG {x}
    | x = INTEGER {x} ;

inst:
    /*| CBEQ ; x = REG ; y = REG  { PIcbeq (x,y) } */
    | CBEQ ; x = REG ; y = int_ou_reg  { PIcbeq (x,y) }
    | J ; s = LABEL { PIj s }
    | LI ; x = REG ; y = INTEGER {PIli (x,y) }
    | INCR ; x = REG ; y = REG {PIincr(x,y) }
    | MODF ; x = REG ; y = REG {PImodf(x,y) }
    | LBI ; x = INTEGER  {PIlbi(x)}
    | LIN ; x = REG ; y = INTEGER { PIlin(x,y) }
    | SO ; x = REG ; y = INTEGER { PIso(x,y) }
    | SD ; x = REG ; y = INTEGER { PIsd(x,y) }
    | CBEQI ; x = REG ; y = INTEGER { PIcbeqi (x,y) } ;


