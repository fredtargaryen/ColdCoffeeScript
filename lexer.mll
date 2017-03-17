(* File lexer.mll *)
{
open Parser        (* The type main_lex is defined in parser.mli *)
}
rule main_lex = parse
	  '#'[^'\n''#']*'#'	{ main_lex lexbuf } (*Skip line comments*)
    | [' ' '\t' '\n']    				{ main_lex lexbuf } (*Skip blanks*)
    | ';'  								{ STMTSEP }
    | ['0'-'9']+ as lxm  				{ INT(int_of_string lxm) }
	| 'L'['1'-'9']['0'-'9']* as lxm		{ IDENT(lxm) }
    | ['A'-'Z']+ as lxm  				{ IDENT(lxm) }
	| '"'(['a'-'z']+|':')'"' as lxm 	{ print_string("[LEX]Found string\n"); STRING(lxm) }
    | "bool"       { BOOLTYPE }
    | "int"        { INTTYPE }
    | "string"     { print_string("[LEX]Found STRINGTYPE\n"); STRINGTYPE }
    | "set"        { SETTYPE }
    | '+'          { PLUS }
    | '-'          { MINUS }
    | '*'          { TIMES }
    | '/'          { DIV }
    | '('          { LPAREN }
    | ')'          { RPAREN }
    | '<'          { LESSTHAN }
    | '>'          { GREATERTHAN }
    | "true"       { TRUE }
    | "false"      { FALSE }
    | "union"      { UNION }
    | "intersect"  { INTERSECT }
    | "concat"     { CONCAT }
    | "difference" { DIFFERENCE }
    | "memberOf"   { MEMBEROF }
    | "=="         { EQUALTO }
    | '='          { print_string("[LEX]Found assignment\n");ASSIGN }
    | "for"        { FOR }
    | "while"      { WHILE }
    | "do"         { DO }
    | "end"        { END }
    | "if"         { IF }
    | "else"       { ELSE }
	| "and"		   { AND }
	| "or" 		   { OR }
	| "not"		   { NOT }
    | eof          { EOF }
	| '{'		   { print_string("[LEX]Found setstart\n");SETSTART }
	| '}'		   { print_string("[LEX]Found setend\n");SETEND }
	| ','		   { print_string("[LEX]Found comma\n");STRINGSEP }
	| "display"	   { print_string("[LEX]Found display\n");DISPLAY }