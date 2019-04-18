open Bigint
open Exptree
open Lexer
open Parser

exception Improper_Closure;;

let compiled_list : opcode list ref = ref [];;

let rec answer_string ans = match ans with
| Num i -> "Integer : "^string_of_int(i)
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
| FunCL (st, oc, gm) -> "Function with Var("^st^")"
| RFunCL (nm, st, oc, gm) -> "Recursive Function with Var("^st^")"
| _ -> raise Improper_Closure;;

let work_out ( (stk, env) : Exptree.closure list * Exptree.gamma) = 
  print_string("\027[1;31mStack Contents\n"); flush stdout;
  let rec work_stack stack = 
    (match stack with [] -> () | cl :: cls -> print_string(closure_string(cl)^"\n"); work_stack cls)
  in work_stack stk;
  print_string("\027[1;32mEnvironment Contents\n"); flush stdout;
  let rec work_env e =
    (match e with [] -> () | (st, cl)::gms -> print_string(st^" : "^closure_string(cl)^"\n"); work_env gms)
  in work_env env;
  print_string("\027[0m")
;;

let _ =
  Printf.printf "Enter Program ($ to execute)\n"; flush stdout;
  let lexbuf = Lexing.from_channel stdin in
  while true do
    try
      try
          Printf.printf "... "; flush stdout;
          compiled_list := ((!compiled_list)@(compile (Parser.exp_parser Lexer.read (lexbuf))));
      with
      | (Read_Definition) -> (compiled_list := ((!compiled_list) @ def_compile (Parser.def_parser Lexer.read (lexbuf))))
      | _ as e -> raise e
    with
    | Lexer.Compiler_Start -> work_out(secd [] [] (!compiled_list) []);
    | Exptree.Not_Found(s) -> print_string("Variable "^s^" not in table (at time of abstraction).\n")
    | (Lexer.Bad_Char s) -> print_string("Illegal Character : "); print_char(s); print_string("\n")
    | Stack_overflow -> print_string("Stack Overflow\n")
    | _  -> print_string("Error\n")
  done
