basic:
	@echo "Compiling"
	@ocamlc -c bigint.ml
	@ocamlc -c exptree.ml
	@ocamlyacc parser.mly
	@ocamlc -c parser.mli parser.ml
	@ocamllex lexer.mll
	@ocamlc -c lexer.ml
	@ocamlc -c interpreter.ml
	@ocamlc -o basic bigint.cmo exptree.cmo parser.cmo lexer.cmo interpreter.cmo 
secd:
	@echo "Compiling"
	@ocamlc -c bigint.ml
	@ocamlc -c exptree.ml
	@ocamlyacc parser.mly
	@ocamlc -c parser.mli parser.ml
	@ocamllex lexer.mll
	@ocamlc -c lexer.ml
	@ocamlc -c interpreter_secd.ml
	@ocamlc -o secd bigint.cmo exptree.cmo parser.cmo lexer.cmo interpreter_secd.cmo 
clear:
	@rm *.c*
	@rm p*.m*[!y] l*.ml
