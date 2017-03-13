(* File lexer.mll *)
{
open Parser        (* The type main_lex is defined in parser.mli *)
}
rule main_lex = parse
      [' ' '\t' '\n']     { main_lex lexbuf }     (* skip blanks *)
    | ';'  			{ STMTSEP }
    | ['0'-'9']+ as lxm  { INT(int_of_string lxm) }
    | ['A'-'Z']+ as lxm  { IDENT(lxm) }
	| '\"'(['a'-'z']+|':')'\"' as lxm { STRING(lxm) }
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