(* File lexer.mll *)
{
open Parser        (* The type token is defined in parser.mli *)
}
rule main_lex = parse
      [' ' '\t']     { main_lex lexbuf }     (* skip blanks *)
    | ['\n' ]  { EOL }
    | ['0'-'9']+ as lxm { INT(int_of_string lxm) }
    | ['a'-'z','A'-'Z']+ as lxm { IDENT(lxm) }
    | "bool"       { BOOLTYPE }
    | "Int"        { INTTYPE }
    | "char"       { CHARTYPE }
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
    | eof          { Eof }
