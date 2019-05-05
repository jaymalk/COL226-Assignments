open Lexer
open Parser
open Procedure


(* INTERPRETER *)
let _ = 
  (* Reading the procedure from the file *)
  (let file = open_in "./example.txt" in
    program := procedure_parser Lexer.read (Lexing.from_channel file));
  (* Starting the interpreter *)
  print_string("Start with calling main.\n"); flush stdout;
  while true do
    print_string("Hi");
    exit(0);
  done
;;