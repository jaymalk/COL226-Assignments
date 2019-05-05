exception Not_implemented

open Exptree
open Bigint
open Parser
open Lexer

open Secd_compiler
open Krivine_compiler

let p1 =  App (Lambda ( "x", Mult ((Integer (mk_big 3)), V "x")), (Integer (mk_big 4)));;
	(*12*)
let p2 = If_Then_Else
    (Cmp ((Integer (mk_big 7))),
     App (Lambda ( "x", Plus ((Integer (mk_big 3)), V "x")), (Integer (mk_big 31))),
     (Integer (mk_big 0)));;
   (*34*)
let p3 = If_Then_Else
    (Cmp ((Integer (mk_big 0))),
     App (Lambda ( "x", Plus ((Integer (mk_big 3)), V "x")), (Integer (mk_big 4))),
     (Integer (mk_big 110)));;
    (*110*)

let p4 = App(Lambda ( "x", App(Lambda ( "y", And(V "x", V "y")), Bool true)), Bool false);;
(*false*)

let p5 = App(Lambda ( "x", App(Lambda ( "y", Or(V "x", V "y")), Bool true)), Bool false);;
(*true*)

let p6 = App(Lambda ( "x", Mult(V "x", App(Lambda ( "x", App(Lambda ( "y", Plus(V "x", V "y")), (Integer (mk_big 4)))), (Integer (mk_big 3))))), (Integer (mk_big 2)));;
(*14*)

let p7 = If_Then_Else(Cmp(App(Lambda ( "x", App(Lambda ( "y", Plus(V "x", V "y")), (Integer (mk_big 4)))), (Integer (mk_big (-5))))), (Integer (mk_big (-29))), App(Lambda ( "x", Plus(V "x", App(Lambda ( "x", Plus(V "x", (Integer (mk_big 1)))), (Integer (mk_big 7))))), (Integer (mk_big 5))));;
(*13*)

let p8 = App(Lambda ( "x", App(Lambda ( "y", Plus(V "x", V "y")), (Integer (mk_big 4)))), App(Lambda ( "x", Mult(V "x", (Integer (mk_big 2)))), (Integer (mk_big 3))));;
(*10*)

let p9 = App(Lambda ( "x", App(Lambda ( "y", Mult(V "x", V "y")), V "x")), (Integer (mk_big 4)));;
(*16*)

let p10 = App(Lambda ( "x", Plus(V "x", App(Lambda ( "x", Mult(V "x", (Integer (mk_big 2)))), App(Lambda ( "x", Plus(V "x", (Integer (mk_big 4)))), (Integer (mk_big 3)))))), (Integer (mk_big 20)));;
(*34*)

let p11 = App(Lambda ( "x", App(Lambda ( "y", And(V "x", V "y")), V "x")), Bool true);;
(*true*)

let p12 = If_Then_Else(Cmp(App(Lambda ( "x", Mult(V "x", (Integer (mk_big 2)))), (Integer (mk_big 4)))), App(Lambda ( "x", App(Lambda ( "y", Or(V "x", V "y")), V "x")), Bool false), Bool true);;
(*false*)

let p13 = App(Lambda ( "x", And(V "x", App(Lambda ( "x", And(V "x", Bool true)), App(Lambda ( "x", And(V "x", Bool true)), Bool true)))), Bool true);;
(*true*)

let p14 = App(Lambda ( "x", And(V "x", App(Lambda ( "x", And(V "x", Bool true)), App(Lambda ( "x", And(V "x", Bool true)), Bool true)))), Bool false);;
(*false*)

let p15 = If_Then_Else(Cmp(App(Lambda ( "x", Mult(V "x", App(Lambda ( "y", V "y"), V "x"))), (Integer (mk_big 1)))), App(Lambda ( "x", Plus(V "x", App(Lambda ( "x", Plus(V "x", (Integer (mk_big 1)))), (Integer (mk_big 3))))), (Integer (mk_big 5))), (Integer (mk_big (-1))));;
(*9*)


(* ADDITIONAL TEST CASES.
                  EXTRA LAYERS IMPLEMENTED.
                                 - RECURSION
                                 - WHILE LOOP (ONLY IN SECD)
                                 - LOCAL DEFINITIONS
                                 - GENERAL DEFINITIONS (IN INTERPRETER/COMPILER)
                                 - ALL OF THE POSSIBLE ARITHMETIC/BOOLEAN FUNCTIONS
*)
let q1 = Parser.exp_parser Lexer.read (Lexing.from_string "let def X = 12 in X*X end;");;
(* 144 *)
let q2a = Parser.exp_parser Lexer.read (Lexing.from_string "let def X = 1 : def Y = 10 : while not Y = 0 { def X = X+X : def Y = Y-1 } in X end;");;
(* 1024 (SECD only) *)
let q3 = Parser.exp_parser Lexer.read (Lexing.from_string "let def Fact = rec func X -> (if X <= 0 then 1 else X*Fact(X-1) fi) in Fact(10) end;");;
(* 3628800 *)
let q4a = Parser.exp_parser Lexer.read (Lexing.from_string "let def Fib = rec func X -> (if X[1/3] <= 0 then X[3/3] else Fib(X[1/3]-1, X[2/3]+X[3/3], X[2/3]) fi) in Fib(10, 1, 0) end;");;
(* 55 (SECD only)*)
let q5a = Parser.exp_parser Lexer.read (Lexing.from_string "let def Fact = rec func X -> (if X <= 0 then 1 else X*Fact(X-1) fi) : def Fib = rec func X -> (if X[1/3] <= 0 then X[3/3] else Fib(X[1/3]-1, X[2/3]+X[3/3], X[2/3]) fi) in Fact(100) + Fib(1000, 1, 0) end;");;
(* 43466557686937456435688527675040625802564660517371873728697173033689237117187908157104370556047519550917391186044217852375202919538848814786447318161282694604454015162057220835139432867704476137795166849228875 (SECD only) *)
let q6 = Parser.exp_parser Lexer.read (Lexing.from_string "let def Fact = rec func X -> (if X <= 0 then 1 else X*Fact(X-1) fi) in (let def Fib = rec func X -> (if X <= 0 then 0 else if X = 1 then 1 else Fib(X-1) + Fib(X-2) fi fi) in Fib(20) + Fact(7) end) end;");;
(* 11805 (Both!) *)


(*Your code will go here*)
(*For thise who have implemented lexer parser, modify the testcases in your grammar and you will have to get those tet_cases at the time of the demo*)

let eval_secd inp = match (secd [] [] (compile inp) []) with
  | (VCL(ans, _)::ls, _) -> ans
  | _ -> raise Not_implemented;;

let eval_krivine inp = match (krivine [] [] (krivine_compile inp)) with
  | (VCL(ans, _), _) -> ans
  | _ -> raise Not_implemented
;;

(*Your code ends*)

let check_secd n inp out = let ans = inp in
  print_string("T" ^ string_of_int(n) ^ " : ");
  try if (ans = out)
    then print_string("Passed\n")
    else print_string("Failed\n");
  with e -> print_endline("Failed : Wrong exception raised : " ^ (Printexc.to_string e))
;;

let check_krivine n inp out = let ans = inp in
  print_string("T" ^ string_of_int(n) ^ " : ");
  try if (ans = out)
    then print_string("Passed\n")
    else print_string("Failed\n");
  with e -> print_endline("Failed : Wrong exception raised : " ^ (Printexc.to_string e))
;;

let print_heading a = print_endline("\n" ^ a);;

(*SECD*)
print_heading "SECD test cases : \n";;

check_secd 1 (eval_secd p1) (Num (mk_big 12));;
check_secd 2 (eval_secd p2) (Num (mk_big 34));;
check_secd 3 (eval_secd p3) (Num (mk_big 110));;
check_secd 4 (eval_secd p4) (Bool false);;
check_secd 5 (eval_secd p5) (Bool true);;
check_secd 6 (eval_secd p6) (Num (mk_big 14));;
check_secd 7 (eval_secd p7) (Num (mk_big 13));;
check_secd 8 (eval_secd p8) (Num (mk_big 10));;
check_secd 9 (eval_secd p9) (Num (mk_big 16));;
check_secd 10 (eval_secd p10) (Num (mk_big 34));;
check_secd 11 (eval_secd p11) (Bool true);;
check_secd 12 (eval_secd p12) (Bool false);;
check_secd 13 (eval_secd p13) (Bool true);;
check_secd 14 (eval_secd p14) (Bool false);;
check_secd 15 (eval_secd p15) (Num (mk_big 9));;
(* New test cases *)
check_secd 101 (eval_secd q1) (Num (mk_big 144));;
check_secd 102 (eval_secd q2a) (Num (mk_big 1024));;
check_secd 103 (eval_secd q3) (Num (mk_big 3628800));;
check_secd 104 (eval_secd q4a) (Num (mk_big 55));;
check_secd 105 (eval_secd q5a) (Num (bigint_of_string "43466557686937456435688527675040625802564660517371873728697173033689237117187908157104370556047519550917391186044217852375202919538848814786447318161282694604454015162057220835139432867704476137795166849228875"));;
check_secd 106 (eval_secd q6) (Num (mk_big 11805));;

(*Krivine*)
print_heading "Krivine test cases : \n";;

check_krivine 1  (eval_krivine p1) (Num (mk_big 12));;
check_krivine 2  (eval_krivine p2) (Num (mk_big 34));;
check_krivine 3  (eval_krivine p3) (Num (mk_big 110));;
check_krivine 4  (eval_krivine p4) (Bool false);;
check_krivine 5  (eval_krivine p5) (Bool true);;
check_krivine 6  (eval_krivine p6) (Num (mk_big 14));;
check_krivine 7  (eval_krivine p7) (Num (mk_big 13));;
check_krivine 8  (eval_krivine p8) (Num (mk_big 10));;
check_krivine 9  (eval_krivine p9) (Num (mk_big 16));;
check_krivine 10 (eval_krivine p10) (Num (mk_big 34));;
check_krivine 11 (eval_krivine p11) (Bool true);;
check_krivine 12 (eval_krivine p12) (Bool false);;
check_krivine 13 (eval_krivine p13) (Bool true);;
check_krivine 14 (eval_krivine p14) (Bool false);;
check_krivine 15 (eval_krivine p15) (Num (mk_big 9));;
(* New test cases *)
check_krivine 101 (eval_krivine q1) (Num (mk_big 144));;
check_krivine 103 (eval_krivine q3) (Num (mk_big 3628800));;
check_krivine 106 (eval_krivine q6) (Num (mk_big 11805));;
