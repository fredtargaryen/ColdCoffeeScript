(* File coffee.ml *)
let _ =
  let lexbuf = Lexing.from_channel stdin in
   while true do
     let result = Parser.main Lexer.main_lex lexbuf in
       print_int 6; print_newline(); flush stdout
   done
