/* File parser.mly */
%{
	let rec pow n m = 
		match m with
			| 0 -> 1
			| m when (m < 0) -> 0
			| m -> n * pow n (m-1)
%}

%token <int> INT
%token <string> IDENT
%token BOOLTYPE INTTYPE STRINGTYPE SETTYPE
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token ASSIGN EQUALTO
%token GREATERTHAN LESSTHAN
%token TRUE FALSE
%token UNION INTERSECT CONCAT DIFFERENCE MEMBEROF
%token FOR WHILE DO END IF ELSE
%token EOL
%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%left MODULO EXPO
%nonassoc UMINUS        /* highest precedence */
%start main             /* the entry point */
%type <int> main

%%
main:
   expr EOL                { $1 }
;

ident:
	IDENT					{ $1 }
;

bool:
	BOOLTYPE				
;

expr:
   INT                     	{ $1 }
 | LPAREN expr RPAREN      	{ $2 }
 | expr PLUS expr          	{ $1 + $3 }
 | expr MINUS expr         	{ $1 - $3 }
 | expr TIMES expr         	{ $1 * $3 }
 | expr DIV expr           	{ $1 / $3 }
 | WHILE bool DO expr END  	{ while $2 do $4 done}
 | ident ASSIGN expr      	{ let $1 = $3 }
 | MINUS expr %prec UMINUS 	{ - $2 }
;
