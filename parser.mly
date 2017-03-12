/* File parser.mly */
%{
	open Coffeeinterpreter
%}

%token <int> INT
%token <string> STRING
%token <string> IDENT
%token BOOLTYPE INTTYPE STRINGTYPE SETTYPE VOIDTYPE
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token ASSIGN EQUALTO
%token GREATERTHAN LESSTHAN
%token TRUE FALSE
%token UNION INTERSECT CONCAT DIFFERENCE MEMBEROF
%token FOR WHILE DO END IF ELSE
%token STMTSEP
%token EOF
%token SETSTART SETEND STRINGSEP
%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%left MODULO EXPO
%nonassoc UMINUS        /* highest precedence */
%start main             /* the entry point */
%type <Coffeeinterpreter.coffeeTerm> main
%type <Coffeeinterpreter.coffeeType> coffeetype
%%
main:
   	statements EOF					  { TmProgram $1 }
;

statements:
  | { [] } /* empty list to match null */
  |	statement STMTSEP statements { $1 :: $3 }	
  | statement STMTSEP { [$1] }
;

statement:
  | IDENT ASSIGN expr { TmAssign ($1, $3) } /* Don't think this works */
  | TRUE { TmBool true } /* Added for testing purposes so there is a "statement" you can put in if/while */
  | IF expr DO statements ELSE statements END { TmIf ($2, $4, $6) }
  | WHILE expr DO statements END { TmWhile ($2, $4) }
;

coffeetype:
    BOOLTYPE	{ BoolType }
  | INTTYPE		{ IntType }
  | STRINGTYPE	{ StringType }
  | VOIDTYPE	{ VoidType }
;

expr:
  /*  TRUE							{ TmBool true } */ /* Temporarily moved to statement for testing */
  | FALSE							{ TmBool false }
  | expr EQUALTO expr 				{ TmEqualTo ($1, $3) }  
  | INT								{ TmInt $1 }
  | expr GREATERTHAN expr			{ TmGreaterThan ($1, $3) }
  | expr LESSTHAN expr				{ TmLessThan ($1, $3) }
  | expr PLUS expr         			{ TmPlus ($1, $3) }
  | expr MINUS expr     	    	{ TmMinus ($1, $3) }
  | expr TIMES expr         		{ TmMult ($1, $3) }
  | expr DIV expr           		{ TmDiv ($1, $3) }
  | STRING							{ TmString $1 }
  |	IDENT							{ TmVar $1 }
  | LPAREN expr RPAREN      		{ $2 } 
/* | MINUS expr %prec UMINUS 		{ TmInt (- $2) } 	*/
/*  | BOOLTYPE expr ASSIGN expr		{ TmAssign ($2, $4) } */
/*  | STRINGTYPE expr ASSIGN expr 	{ TmAssign ($2, $4) } */
/*  | INTTYPE expr ASSIGN expr		{ TmAssign ($2, $4) } */
;
