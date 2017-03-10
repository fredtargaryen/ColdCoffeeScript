(* File coffee.ml *)
open Lexer
open Parser
open Coffeeinterpreter
open Arg

let parseProgram c = 
	(*lexbuf is the buffer the lexer reads from*)
    try let lexbuf = Lexing.from_channel c in  
			(*Parse the result of lexing lexbuf*)
            main main_lex lexbuf 
    with Parsing.Parse_error -> failwith "Parse failure!" ;;
	
let arg = ref stdin in
let setProg p = arg := open_in p in
let usage = "./main PROGRAM_FILE" in
parse [] setProg usage ; 
let parsedProg = parseProgram !arg in
let () = print_string "Program Parsed" ; print_newline() in
let _ = typeProg parsedProg in
let () = print_string "Program Type Checked" ; print_newline() in
let result = bigEval parsedProg in
let () = print_string "Program Evaluated using big step semantics to ==> "; print_res result; print_newline() in
flush stdout
