basic:
	@make setup
	@ocamlc -c interpreter.ml
	@ocamlc -o basic bigint.cmo exptree.cmo parser.cmo lexer.cmo interpreter.cmo 
secd:
	@make setup
	@ocamlc -c secd.ml
	@ocamlc -o secd bigint.cmo exptree.cmo parser.cmo lexer.cmo secd.cmo 
krivine:
	@make setup
	@ocamlc -c krivine.ml
	@ocamlc -o krivine bigint.cmo exptree.cmo parser.cmo lexer.cmo krivine.cmo 
setup:
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
