/* File parser.mly */
%{
	open coffeeInterpreter
%}

%token <int> INT
%token <string> STRING
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
%token EOF
%token SETSTART SETEND STRINGSEP
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
	TRUE					{ TmBool true }
  | FALSE					{ TmBool false }    
  | int GREATERTHAN int		{ TmBool ($1 > $3) }
  | int LESSTHAN int		{ TmBool ($1 < $3) }
  | int EQUALTO int 		{ TmBool ($1 == $3) }
;

string:
	STRING					{ TmString $1 }
;

int:
   INT						{ TmInt $1 }
 | int PLUS int          	{ TmInt ($1 + $3) }
 | int MINUS int         	{ TmInt ($1 - $3) }
 | int TIMES int         	{ TmInt ($1 * $3) }
 | int DIV int           	{ TmInt ($1 / $3) }
 | MINUS int %prec UMINUS 	{ TmInt (- $2) }
;

ident:
	IDENT							{ TmVar $1 }
;

expr:
   LPAREN expr RPAREN      			{ $2 }
 | WHILE bool DO expr END  			{ TmWhile ($2, $4) }
 | BOOLTYPE ident ASSIGN bool		{ TmAssign ($2, $4) }
 | STRINGTYPE ident ASSIGN string 	{ TmAssign ($2, $4) }
 | INTTYPE ident ASSIGN int			{ TmAssign ($2, $4) }
;
