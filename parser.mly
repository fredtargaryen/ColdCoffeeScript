/* File parser.mly */
%{
	open CoffeeInterpreter

  exception MissingSeperatorError of string
%}

%token <string> IDENT /*types*/
%token <string> STRING
%token <int> INT
%token BOOLTYPE INTTYPE STRINGTYPE SETTYPE VOIDTYPE
%token PLUS MINUS TIMES DIV /*arithmetic*/
%token NOT AND OR /*logic*/
%token LPAREN RPAREN /*brackets*/
%token ASSIGN EQUALTO GREATERTHAN LESSTHAN
%token TRUE FALSE
%token UNION INTERSECT CONCAT DIFFERENCE MEMBEROF /*Set operations*/
%token WHILE DO END IF ELSE /*if and while*/
%token DISPLAY
%token SETSTART SETEND STRINGSEP/*set literals*/
%token STMTSEP
%token EOF
%left ASSIGN EQUALTO GREATERTHAN LESSTHAN OR MEMBEROF/* lowest precedence */
%left STRINGSEP
%left PLUS MINUS AND UNION INTERSECT DIFFERENCE
%left TIMES DIV CONCAT       /* medium precedence */
%nonassoc IDENT NOT       /* highest precedence */
%start main             /* the entry point */
%type <CoffeeInterpreter.coffeeTerm> main
%type <CoffeeInterpreter.coffeeType> coffeetype
%%
main:
   	statements EOF					  { TmProgram $1 }
;

statements:
  | { [] } /* empty list to match null */
  |	statement STMTSEP statements { $1 :: $3 }
  | statement {raise (MissingSeperatorError "One of your-a statements is just a concept. Add a semicolon or you will-a fail!")}
;

statement:
  | coffeetype IDENT ASSIGN expr 				{ TmAssign ($1, $2, $4) }
  | IDENT ASSIGN expr 							{ TmReAssign ($1, $3) }
  | IF expr DO statements ELSE statements END 	{ TmIfElse ($2, $4, $6) }
  | IF expr DO statements END 					{ TmIf ($2, $4) }
  | WHILE expr DO statements END 				{ TmWhile ($2, $4) }
  | DISPLAY expr expr							{ TmDisplay ($2, $3) }
;

coffeetype:
    BOOLTYPE	{ BoolType }
  | INTTYPE		{ IntType }
  | STRINGTYPE	{ StringType }
  | SETTYPE		{ SetType }
  | VOIDTYPE	{ VoidType }
;

set_terms:
    expr						{ [$1] }
  | expr STRINGSEP set_terms	{ $1 :: $3 }
;

expr:
    TRUE							{ TmBool true }
  | FALSE							{ TmBool false }
  | expr EQUALTO expr 				{ TmEqualTo ($1, $3) }  
  | INT								{ TmInt $1 }
  | expr GREATERTHAN expr			{ TmGreaterThan ($1, $3) }
  | expr LESSTHAN expr				{ TmLessThan ($1, $3) }
  | expr PLUS expr         			{ TmPlus ($1, $3) }
  | expr MINUS expr     	    	{ TmMinus ($1, $3) }
  | expr TIMES expr         		{ TmMult ($1, $3) }
  | expr DIV expr           		{ TmDiv ($1, $3) }
  | STRING							{ TmString (String.sub $1 1 (String.length $1 - 2)) }
  |	IDENT							{ TmVar $1 }
  | SETSTART SETEND					{ TmSetLiteral [] }
  | SETSTART set_terms SETEND		{ TmSetLiteral $2 }
  | LPAREN expr RPAREN      		{ $2 } 
  | NOT expr						{ TmNot $2 }
  | expr AND expr 					{ TmAnd ($1, $3) }
  | expr OR expr					{ TmOr ($1, $3) }
  | expr UNION expr					{ TmUnion ($1, $3) }
  | expr INTERSECT expr				{ TmIntersect ($1, $3) }
  | expr CONCAT expr				{ TmConcat ($1, $3) }
  | expr DIFFERENCE expr 			{ TmDifference ($1, $3) }
  | expr MEMBEROF expr				{ TmMemberOf ($1, $3) }
;
