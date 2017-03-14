(* File coffee.ml *)
exception IDoNotUnderstandAWhatAYouAreASaying;;

open Lexer
open Parser
open Coffeeinterpreter
open Language
open Arg
open Str

let parseProgram c = 
	(*lexbuf is the buffer the lexer reads from*)
    try let lexbuf = Lexing.from_channel c in  
			(*Parse the result of lexing lexbuf*)
            main main_lex lexbuf 
    with Parsing.Parse_error -> failwith "Parse failure!";;

(*A string can be : or at least one character a-z*)
let word_regexp_str = ":|['a'-'z']+";;
(*A set can be the empty set {}, or have at least one word separated with a ,*)
let set_regexp = regexp ("^{}$|^{(" ^ word_regexp_str ^ ",)*(" ^ word_regexp_str ^ ")}$");;

let rec get_lists i = 
	(*Read the next line from stdin*)
	let next_line = read_line () in
		try
			(*Probably a set. Check against set_regexp*)
			(if (string_match set_regexp next_line 0) then 
				(*L1 to Ln are the input sets*) (*Split the words up using commas; convert resulting list to set*) (*Join tuple of the two to recursive call*)
				("L"^(string_of_int i), TmSet (of_list_input (split (regexp ",") next_line))) :: get_lists (i + 1)
			else
				(*Probably the int K. Try converting to int*)
				let k = int_of_string next_line in
					(if (k < 0) 
						then raise IDoNotUnderstandAWhatAYouAreASaying "k ees-a too smol. Must be at-a least tsero."
					else [("K", TmInt k)]))
		(*Not a valid set or int*)
		with Failure "int_of_string" -> raise IDoNotUnderstandAWhatAYouAreASaying "You 'ave not-a formatted-a your input correctly.";;
						
let progFile = ref stdin in
let setProg p = progFile := open_in p in
let usage = "./main PROGRAM_FILE" in
parse [] setProg usage ; 
let parsedProg = parseProgram !progFile in
let () = print_string "Program-a Parsed" ; print_newline() in
let _ = typeProg parsedProg in
let () = print_string "Program-a Type Checked. Give-a me your inputs" ; print_newline() in
let initialEnv = get_lists 1 in
let result = eval initialEnv parsedProg in
let () = print_string "Program Evaluated using big step semantics to ==> "; print_res result; print_newline() in
flush stdout
