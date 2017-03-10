all :
	ocamlc -c coffeeinterpreter.ml
	ocamllex lexer.mll
	ocamlyacc parser.mly
	ocamlc -c parser.mli
	ocamlc -c lexer.ml
	ocamlc -c parser.ml
	ocamlc -c coffee.ml
	ocamlc -o coffeet lexer.cmo parser.cmo coffee.cmo coffeeinterpreter.cmo