open Bigint
open Krivine_compiler
open Exptree
open Lexer
open Parser

exception Improper_Closure;;

let compiled_list : precode list ref = ref [];;
let environment : gamma ref = ref [];;

let rec answer_string ans = match ans with
| Num i -> "Integer : "^print_num(i)
| Bool b -> "Bool : "^string_of_bool(b)
| Tup (n, al) ->
  (let rec tuple_ans alist str = match alist with
    | [] -> "()"
    | x :: [] -> answer_string(x)^")"
    | x :: xs -> tuple_ans xs (str^answer_string(x)^", ")
   in tuple_ans al "(")
;;


let rec closure_string (cl : closure) = match cl with
| VCL (an, gm) -> answer_string(an)
| CL (_, _) -> "Unevaluated Closure"
| FCL (st, pc, gm) -> "Function with Var("^st^")"
| _ -> raise Improper_Closure;;

let rec process (env : gamma) = 
  let rec remove_extras (x) (e') = match e' with
  [] -> e'
  | cl :: cls -> if (fst cl = x) then (remove_extras x cls) else cl::(remove_extras x cls)
  in match env with
| [] -> env
| x :: xs -> x::process(remove_extras (fst x) env)
;;

let rec work (cls, env) =
  environment := process(env);
  print_string("\027[1;31mAnswer : "^closure_string(cls)^"\n\027[0m")
;;

let _ =
  Printf.printf "Enter Program ($ to execute)\n"; flush stdout;
  let lexbuf = Lexing.from_channel stdin in
  while true do
    try
      try
        Printf.printf "... "; flush stdout;
        compiled_list := ((!compiled_list)@(krivine_compile (Parser.exp_parser Lexer.read (lexbuf)))); (Lexing.new_line lexbuf);
      with
      | Lexer.Compiler_Start -> work(krivine [] (!environment) (!compiled_list));  (compiled_list := []); print_string("\027[0m")
      | (Read_Definition) -> (compiled_list := ((!compiled_list) @ [krivine_def_compile (Parser.def_parser Lexer.read (lexbuf))] )); (Lexing.new_line lexbuf);
      | _ as e -> raise e
    with
    | Exptree.Not_Found(s) -> print_string("\027[1;33mVariable "^s^" not in table (at time of abstraction).\027[0m\n"); (compiled_list := []);
    | (Lexer.Bad_Char s) -> print_string("Illegal Character : "); print_char(s); print_string("\n"); Lexing.flush_input lexbuf;
    | Stack_overflow -> print_string("Stack Overflow\n")
    | _  -> print_string("Error\n"); Lexing.flush_input lexbuf; (compiled_list := []);
  done
