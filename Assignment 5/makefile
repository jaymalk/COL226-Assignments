all:
	@echo "Compiling"
	@ocamlc -c bigint.ml
	@ocamlc -c exptree.ml
	@ocamlyacc parser.mly
	@ocamlc -c parser.mli parser.ml
	@ocamllex lexer.mll
	@ocamlc -c lexer.ml
clear:
	@rm *.c*
	@rm p*.m*[!y] l*.ml