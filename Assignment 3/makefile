make:
	# Bigint
	ocamlc -o a0.mli
	ocamlc -o a0.ml
	# Expression Tree
	ocamlc -o a1.mli
	ocamlc -o a1.ml
	# Lexer
	ocamllex a2.mll
	# Parser
	ocamlyacc a3.mly
build:
	ocamlbuild a0.cmo a1.cmo a2.cmo a3.cmo
clean:
	rm a.out a2.ml a3.mli a3.ml
	rm *.c*
