(* File lexer.mll *)
{
open Parser        (* The type main_lex is defined in parser.mli *)

let lineNum = ref 1

exception SyntaxError of string
}
rule main_lex = parse
	  '#'[^'\n''#']*'#'	{ main_lex lexbuf } (*Skip line comments*)
    | [' ' '\t']    				{ main_lex lexbuf } (*Skip blanks*)
    | '\n'  {incr lineNum; main_lex lexbuf}
    | ';'  								{ STMTSEP }
    | ['0'-'9']+ as lxm  				{ INT(int_of_string lxm) }
	| 'L'['1'-'9']['0'-'9']* as lxm		{ IDENT(lxm) }
    | ['A'-'Z']+ as lxm  				{ IDENT(lxm) }
	| '"'(['a'-'z']+|':')'"' as lxm 	{ STRING(lxm) }
    | "bool"       { BOOLTYPE }
    | "int"        { INTTYPE }
    | "string"     { STRINGTYPE }
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
    | '='          { ASSIGN }
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
	| '{'		   { SETSTART }
	| '}'		   { SETEND }
	| ','		   { STRINGSEP }
	| "display"	   { DISPLAY }
    | _  { raise (SyntaxError ("Unexpected term: " ^ Lexing.lexeme lexbuf ^ " on line: " ^ (string_of_int !lineNum) ^ ". Were you trying to order a comment?")) }