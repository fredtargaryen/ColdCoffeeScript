all :
	ocamllex lexer.mll
	ocamlyacc parser.mly
	ocamlc -c parser.mli
	ocamlc -c lexer.ml
	ocamlc -c parser.ml
	ocamlc -c calc.ml
	ocamlc -o calct lexer.cmo parser.cmo calc.cmo