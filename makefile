all :
	ocamlc -c language.ml
	ocamlc -c coffeeinterpreter.ml
	ocamllex lexer.mll
	ocamlyacc parser.mly
	ocamlc -c parser.mli
	ocamlc -c lexer.ml
	ocamlc -c parser.ml
	ocamlc -c coffee.ml
	ocamlc -o coffeet language.cmo coffeeinterpreter.cmo lexer.cmo parser.cmo coffee.cmo