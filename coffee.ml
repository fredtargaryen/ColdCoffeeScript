(* File coffee.ml *)
exception IDoNotUnderstandAWhatAYouAreASaying;;
exception KEesATuSmol;;

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
	
(*A set can be the empty set {}, or have at least one word separated with a ,*)
let set_regexp = regexp "{}\\|{\\(\\(:\\|\\([a-z]+\\)\\),\\)*\\(:\\|[a-z]+\\)}";;

let rec get_lists i = 
	(*Read the next line from stdin. Ignore whitespace*)
	let next_line = Str.global_replace (regexp "\t\\| ") "" (read_line ()) in
		try
			(*Probably a set. Check against set_regexp*)
			(if (string_match set_regexp next_line 0) then 
				(*Join tuple of the language name and set to recursive call*) 
				(
					(*L1 to Ln are the input sets*)
					"L"^(string_of_int i), 
					(*Split the words up using commas; convert resulting list to set*)
					TmSet (of_list_input (split 
												(regexp ",") 
												(*Remove curly braces before splitting*)
												(String.sub next_line 1 (String.length next_line - 2))))
				) :: get_lists (i + 1)
			else
				(*Probably the int K. Try converting to int*)
				let k = int_of_string next_line in
					(if (k < 0) 
						then raise KEesATuSmol
					else [("K", TmInt k)]))
		(*Not a valid set or int*)
		with Failure "int_of_string" -> raise IDoNotUnderstandAWhatAYouAreASaying;;
						
let rec input_type_check l =
	try
		match l with
			[] -> []
		  | (var, term) :: tail -> 
				(match term with 
					TmSet (s) -> (var, SetType) :: input_type_check tail
				  | TmInt (i) -> [(var, IntType)]
				  | _ -> raise IDoNotUnderstandAWhatAYouAreASaying)
	with TypeError -> raise IDoNotUnderstandAWhatAYouAreASaying;;
	  
	  
let progFile = ref stdin in
let setProg p = progFile := open_in p in
let usage = "./main PROGRAM_FILE" in
parse [] setProg usage ; 
let parsedProg = parseProgram !progFile in
let () = print_string "Program-a Parsed. Give-a me your inputs"; print_newline() in
let progEnv = get_lists 1 in
let typeEnv = input_type_check progEnv in
let _ = typeProg typeEnv parsedProg in
let () = print_string "Program-a Type Checked\nOutput:"; print_newline() in
let _ = eval progEnv parsedProg in
(*let () = print_string "Program Evaluated using big step semantics to ==> "; print_res result; print_newline() in*)
let () = print_string "But thees ees-a just a concept"; print_newline() in
flush stdout
