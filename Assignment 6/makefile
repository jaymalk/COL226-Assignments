all:
	@echo "Compiling"
	@ocamlc -c procedure.ml
	@ocamlyacc parser.mly
	@ocamlc -c parser.mli parser.ml
	@ocamllex lexer.mll
	@ocamlc -c lexer.ml
clear:
	@rm *.c*
	@rm pa*.m*[!y] l*.ml
