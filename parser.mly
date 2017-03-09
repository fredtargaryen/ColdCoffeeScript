/* File parser.mly */
%{
	let rec pow n m = 
		match m with
			| 0 -> 1
			| m when (m < 0) -> 0
			| m -> n * pow n (m-1)
%}

%token <int> INT
%token <string> STRING
%token IDENT
%token BOOLTYPE INTTYPE STRINGTYPE SETTYPE
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token ASSIGN EQUALTO
%token GREATERTHAN LESSTHAN
%token TRUE FALSE
%token UNION INTERSECT CONCAT DIFFERENCE MEMBEROF
%token FOR WHILE DO END IF ELSE
%token EOL
%token EOF
%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%left MODULO EXPO
%nonassoc UMINUS        /* highest precedence */
%start main             /* the entry point */
%type <unit> main

%%
main:
   expr EOF                 { $1 }
;

bool:
	TRUE					{ true }
  | FALSE					{ false }
  | int GREATERTHAN int		{ $1 > $3 }
  | int LESSTHAN int		{ $1 < $3 }
  | int EQUALTO int 		{ $1 == $3 }
;

string:
	STRING					{ $1 }
;

int:
   INT						{ $1 }
 | int PLUS int          	{ $1 + $3 }
 | int MINUS int         	{ $1 - $3 }
 | int TIMES int         	{ $1 * $3 }
 | int DIV int           	{ $1 / $3 }
 | MINUS int %prec UMINUS 	{ - $2 }
;

expr:
   LPAREN expr RPAREN      			{ $2 }
 | WHILE bool DO expr END  			{ while $2 do $4 done}
 | BOOLTYPE IDENT ASSIGN bool		{ let $2 = $4 }
 | STRINGTYPE IDENT ASSIGN string 	{ let $2 = $4 }
 | INTTYPE IDENT ASSIGN int			{ let $2 = $4 }
;
