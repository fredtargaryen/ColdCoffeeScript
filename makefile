all :
	ocamlc -c language.ml
	ocamlc -c coffeeInterpreter.ml
	ocamllex lexer.mll
	ocamlyacc parser.mly
	ocamlc -c parser.mli
	ocamlc -c lexer.ml
	ocamlc -c parser.ml
	ocamlc -c coffee.ml
	ocamlc -o mysplinterpreter str.cma language.cmo coffeeInterpreter.cmo lexer.cmo parser.cmo coffee.cmo