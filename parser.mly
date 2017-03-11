/* File parser.mly */
%{
	open Coffeeinterpreter
%}

%token <int> INT
%token <string> STRING
%token <string> IDENT
%token BOOLTYPE INTTYPE STRINGTYPE SETTYPE ASSIGNTYPE
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token ASSIGN EQUALTO
%token GREATERTHAN LESSTHAN
%token TRUE FALSE
%token UNION INTERSECT CONCAT DIFFERENCE MEMBEROF
%token FOR WHILE DO END IF ELSE
%token EOL
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
  |	statement EOL statements { $1 :: $3 }	
  | statement { $1 :: []}
  | EOL { [] }
;

statement:
  | IDENT ASSIGN expr { TmAssign ($1, $3) } /* Don't think this works */
  | TRUE { TmBool true } /* Added for testing purposes so there is a "statement" you can put in if/while */
  | IF expr DO statements ELSE statements { TmIf ($2, $4, $6) }
  | WHILE expr DO statements END { TmWhile ($2, $4) }
;

coffeetype:
    BOOLTYPE	{ BoolType }
  | INTTYPE		{ IntType }
  | STRINGTYPE	{ StringType }
  | ASSIGNTYPE	{ AssignType }
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
 /* | WHILE expr DO expr END  		{ TmWhile ($2, $4) } */
/* | MINUS expr %prec UMINUS 		{ TmInt (- $2) } 	*/
/*  | BOOLTYPE expr ASSIGN expr		{ TmAssign ($2, $4) } */
/*  | STRINGTYPE expr ASSIGN expr 	{ TmAssign ($2, $4) } */
/*  | INTTYPE expr ASSIGN expr		{ TmAssign ($2, $4) } */
;
