all:
	@echo "Compiling"
	@ocamlc -c procedure.ml
	@ocamlyacc parser.mly
	@ocamlc -c parser.mli parser.ml
	@ocamllex lexer.mll
	@ocamlc -c lexer.ml
	@ocamlc -c interpreter.ml
	@ocamlc -o run procedure.cmo parser.cmo lexer.cmo interpreter.cmo 
clear:
	@rm *.c*
	@rm pa*.m*[!y] l*.ml
