open Bigint
open Exptree
open Secd_compiler
open Lexer
open Parser

exception Improper_Closure;;

let compiled_list : opcode list ref = ref [];;
let environment : gamma ref = ref [];;

let rec answer_string ans = match ans with
| Num i -> "Integer : "^print_num(i)
| Bool b -> "Bool : "^string_of_bool(b)
| Tup (n, al) ->
  (let rec tuple_ans alist str = match alist with
    | [] -> "()"
    | x :: [] -> str^answer_string(x)^")"
    | x :: xs -> tuple_ans xs (str^answer_string(x)^", ")
   in tuple_ans al "Tuple : (")
;;


let rec closure_string (cl : closure) = match cl with
| VCL (an, gm) -> answer_string(an)
| FunCL (st, oc, gm) -> "Function with Var("^st^")"
| RFunCL (nm, st, oc, gm) -> "Recursive Function with Var("^st^")"
;;

let rec process (env : gamma) = 
  let rec remove_extras (x) (e') = match e' with
  [] -> e'
  | cl :: cls -> if (fst cl = x) then (remove_extras x cls) else cl::(remove_extras x cls)
  in match env with
| [] -> env
| x :: xs -> x::process(remove_extras (fst x) env)
;;

let work_out ( (stk, env) : closure list * gamma) = 
  environment := process(env);
  print_string("\027[1;31mStack Contents\n"); flush stdout;
  let rec work_stack stack = 
    (match stack with [] -> () | cl :: cls -> print_string(closure_string(cl)^"\n"); work_stack cls)
  in work_stack stk;
  print_string("\027[1;32mEnvironment Contents\n"); flush stdout;
  let rec work_env e =
    (match e with [] -> () | (st, cl)::gms -> print_string(st^" : "^closure_string(cl)^"\n"); work_env gms)
  in work_env (!environment);
  print_string("\027[0m")
;;

let from_file (filename : string) =
  let file = open_in filename in
    while true do
      let line = input_line file in
      try
        try
          compiled_list := ((!compiled_list)@(compile (Parser.exp_parser Lexer.read (Lexing.from_string line))));
        with
        | Lexer.Compiler_Start -> work_out(secd [] (!environment) (!compiled_list) []); exit(0);
        | (Parsing.Parse_error) -> (compiled_list := ((!compiled_list) @ def_compile (Parser.def_parser Lexer.read (Lexing.from_string line))));
        | _ as e -> raise e
      with
      | Exptree.Not_Found(s) -> print_string("\027[1;33mVariable "^s^" not in table (at time of abstraction).\027[0m\n"); exit(0);
      | (Lexer.Bad_Char s) -> print_string("Illegal Character : "); print_char(s); print_string("\n"); exit(0);
      | Stack_overflow -> print_string("Stack Overflow\n"); exit(0);
      | _  as e-> raise e;
    done
;;

let interpreter () =
  Printf.printf "Enter Program ($ to execute)\n"; flush stdout;
  let lexbuf = Lexing.from_channel stdin in
  while true do
    try
      try
          Printf.printf "... "; flush stdout;
          compiled_list := ((!compiled_list)@(compile (Parser.exp_parser Lexer.read (lexbuf)))); (Lexing.new_line lexbuf);
      with
      | Lexer.Compiler_Start -> work_out(secd [] (!environment) (!compiled_list) []); (compiled_list := []); Lexing.flush_input lexbuf;
      | (Read_Definition) -> (compiled_list := ((!compiled_list) @ def_compile (Parser.def_parser Lexer.read (lexbuf)))); (Lexing.new_line lexbuf);
      | _ as e -> raise e
    with
    | Exptree.Not_Found(s) -> print_string("\027[1;33mVariable "^s^" not in table (at time of abstraction).\027[0m\n"); (compiled_list := []);
    | (Lexer.Bad_Char s) -> print_string("Illegal Character : "); print_char(s); print_string("\n"); Lexing.flush_input lexbuf;
    | Stack_overflow -> print_string("Stack Overflow\n")
    | _  -> print_string("Error\n"); Lexing.flush_input lexbuf; (compiled_list := []);
  done
;;

let _ = if (Array.length Sys.argv = 2) then from_file (Sys.argv.(1)) else interpreter()
;;