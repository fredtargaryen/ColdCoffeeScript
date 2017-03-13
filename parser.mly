/* File parser.mly */
%{
	open Coffeeinterpreter
%}

%token <int> INT
%token <string> STRING
%token <string> IDENT
%token BOOLTYPE INTTYPE STRINGTYPE SETTYPE VOIDTYPE
%token PLUS MINUS TIMES DIV
%token NOT AND OR
%token LPAREN RPAREN
%token ASSIGN EQUALTO GREATERTHAN LESSTHAN
%token TRUE FALSE
%token UNION INTERSECT CONCAT DIFFERENCE MEMBEROF
%token FOR WHILE DO END IF ELSE
%token STMTSEP
%token EOF
%token DISPLAY
%token SETSTART SETEND STRINGSEP
%left ASSIGN EQUALTO GREATERTHAN LESSTHAN OR/* lowest precedence */
%left PLUS MINUS AND 
%left TIMES DIV NOT        /* medium precedence */
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
  | coffeetype IDENT ASSIGN expr { TmAssign ($1, $2, $4) }
  | TRUE { TmBool true } /* Added for testing purposes so there is a "statement" you can put in if/while */
  | IF expr DO statements ELSE statements END { TmIfElse ($2, $4, $6) }
  | IF expr DO statements END { TmIf ($2, $4) }
  | WHILE expr DO statements END { TmWhile ($2, $4) }
  | DISPLAY expr expr				{ TmDisplay ($2, $3) }
;

coffeetype:
    BOOLTYPE	{ BoolType }
  | INTTYPE		{ IntType }
  | STRINGTYPE	{ StringType }
  | SETTYPE		{ SetType }
  | VOIDTYPE	{ VoidType }
;

strings:
								{ [] }
  | STRING						{ [$1] }
  | STRING STRINGSEP strings	{ $1 :: $3 }
;;

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
  | SETSTART strings SETEND			{ TmSetLiteral $2 }
  | LPAREN expr RPAREN      		{ $2 } 
  | NOT expr						{ TmNot $2 }
  | expr AND expr 					{ TmAnd ($1, $3) }
  | expr OR expr					{ TmOr ($1, $3) }
  | expr UNION expr					{ TmUnion ($1, $3) }
  | expr INTERSECT expr				{ TmIntersect ($1, $3) }
  | expr CONCAT expr				{ TmConcat ($1, $3) }
  | expr DIFFERENCE expr 			{ TmDifference ($1, $3) }
  | expr MEMBEROF expr				{ TmMemberOf ($1, $3) }
/* | MINUS expr %prec UMINUS 		{ TmInt (- $2) } 	*/
;
