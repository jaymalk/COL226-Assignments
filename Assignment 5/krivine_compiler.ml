open Bigint
open Exptree

(* ================================================ *)
(* KRIVINE MACHINE *)
(* ================================================ *)

(* Krivine Compiler Codes *)
type precode = INT of bigint | VR of string | BOOL of bool | NULL | EXT
| ABSO of precode list | NEGA of precode list
| ADD of (precode list) * (precode list) | SUB of (precode list) * (precode list) | DVD of (precode list) * (precode list) | MUL of (precode list) * (precode list) | MOD of (precode list) * (precode list)
| NT of precode list | AND of (precode list) * (precode list) | OR of (precode list) * (precode list) 
| CMPE of (precode list) | EQ | GTNE | LTNE | GTN | LTN
| CONDI of (precode list) * (precode list) | LETK of precode * (precode list) | RETK
| LAM of string * (precode list) | RLAM of string * string * (precode list) | APP of (precode list)
| DF of string * (precode list)

(* ------------------------------------------------------------------------ *)
(* ANSWER, GAMMA and CLOSURE TYPES | FOR SECD MACHINE *)
type answer = Num of bigint | Bool of bool | Tup of int * (answer list)
and gamma = (string * closure) list
and closure = VCL of (answer * gamma) | CL of (precode list * gamma) 
            | FCL of (string * precode list * gamma) | RFCL of (string * string * precode list * gamma)
(* ------------------------------------------------------------------------ *)

(* Function for accessing gamma elements *)
let rec get ((st, gm) : (string * gamma)) = match gm with
[] -> raise (Not_Found(st));
| x :: xs -> if (fst x)=st then (snd x) else get(st, xs)
;; 


(* Krivine Compiler *)
let rec krivine_compile (ex : exptree) = match ex with
  (* Handle commands *)
    | Exit -> [EXT]
    | Null -> [NULL]
  (* Basic types *)
    | Integer(x) -> [INT x]
    | V st -> [VR st]
    | Bool b -> [BOOL b]
  (* Unary operations : Integers *)
    | Abs(x) -> [ABSO(krivine_compile x)]
    | Negative(x) -> [NEGA(krivine_compile x)]
  (* Binary operations : Integers *)
    | Plus(x1, x2) -> [ADD(krivine_compile x1, krivine_compile x2)]
    | Minus(x1, x2) -> [SUB(krivine_compile x1, krivine_compile x2)]
    | Mult(x1, x2) -> [MUL(krivine_compile x1, krivine_compile x2)]
    | Div(x1, x2) -> [DVD(krivine_compile x1, krivine_compile x2)]
    | Rem(x1, x2) -> [MOD(krivine_compile x1, krivine_compile x2)]
  (* Unary operations : Bool *)
    | Not(b) -> [NT(krivine_compile b)]
  (* Binary operations : Bool *)
    | Or(b1, b2) -> [OR(krivine_compile b1, krivine_compile b2)]
    | And(b1, b2) -> [AND(krivine_compile b1, krivine_compile b2)]
  (* Comparison operations *)
    | Cmp(x) -> [CMPE(krivine_compile x)]
    | Equals(e1, e2) ->    (krivine_compile e1) @ (krivine_compile e2) @ [EQ]
    | GreaterTE(e1, e2) -> (krivine_compile e1) @ (krivine_compile e2) @ [GTNE]
    | LessTE(e1, e2) ->    (krivine_compile e1) @ (krivine_compile e2) @ [LTNE]
    | GreaterT(e1, e2) ->  (krivine_compile e1) @ (krivine_compile e2) @ [GTN]
    | LessT(e1, e2) ->     (krivine_compile e1) @ (krivine_compile e2) @ [LTN]
  (* Conditional *)
    | If_Then_Else(e1, e2, e3) -> krivine_compile e1 @ [CONDI(krivine_compile e2, krivine_compile e3)]
  (* Parenthesis *)
    | InParen(e) -> (krivine_compile e)
  (* Let statements in expressions *)
    | Let(d, e) -> [LETK(krivine_def_compile d, (krivine_compile e)@[RETK])]
  (* Function Abstraction *)
    | Lambda(st, e1) -> [LAM(st, krivine_compile e1)]
  (* Recursive Function Abstraction *)
    | RecFuncAbs(nm, st, e1) -> [RLAM(nm, st, krivine_compile e1)]
  (* Function Call *)
    | App(e1, e2) -> (krivine_compile e1) @ [APP(krivine_compile e2)]
  (* Categories not implemented *)
    | _ -> raise Not_implemented
(* Definitions compiler *)
and krivine_def_compile (d : definition) = match d with
    | SimpleType(st, e) -> DF(st, krivine_compile e)
    | _ -> raise Not_implemented
;;

(* Krivine Solver *)
let rec krivine (stack : closure list) (env : gamma) (pc : precode list) = match pc with
  (* Higher order precodes, for 4th wall interaction *)
    | EXT :: pc' -> exit 0
    | NULL :: pc' -> (krivine stack env pc')
  (* Process Complete *)
    | [] -> (List.hd stack, env)
    | RETK::pc'  -> (List.hd stack, env)
  (* Basic types *)
    | INT(x) :: pc' -> krivine ((VCL(Num x, env))::stack) env pc'
    | VR(x) :: pc' -> (match get(x, env) with CL(pc, env') -> (let x' = fst (krivine [] env' pc) in krivine (x'::stack) env pc')| _ as x -> krivine (x::stack) env pc')
    | BOOL(b) :: pc' -> krivine (VCL(Bool b, env)::stack) env pc'
  (* Unary operations : Integers *)
    (* Absolute value *)
    | ABSO(e) :: pc' -> (match e with
            | [NULL] -> (match stack with
                            | VCL(Num x, e')::stack' -> krivine (VCL(Num (abs x), e')::stack') env pc' 
                            | _ -> raise BadStack)
            | _ -> krivine stack env (e @ [ABSO([NULL])] @ pc') )
    (* Negation *)
    | NEGA(e) :: pc' -> (match e with
            | [NULL] -> (match stack with
                            | VCL(Num x, e')::stack' -> krivine (VCL(Num (minus x), e')::stack') env pc' 
                            | _ -> raise BadStack)
            | _ -> krivine stack env (e @ [NEGA([NULL])] @ pc') )
  (* Binary operations : Integers *)
    (* Addition *)
    | ADD(e1, e2) :: pc' ->   (match (e1, e2) with 
            | ([NULL], [NULL]) -> (match stack with 
                            | VCL(Num x2, e')::VCL(Num x1, e'')::stack' -> krivine (VCL(Num (add x2 x1), e'')::stack') env pc' 
                            | _ -> raise BadStack)
            | ([NULL], _) -> krivine stack env (e2 @ [ADD([NULL], [NULL])] @ pc') 
            | (_, _) -> krivine stack env (e1 @ [ADD([NULL], e2)] @ pc'))
    (* Subtraction *)
    | SUB(e1, e2) :: pc' ->   (match (e1, e2) with 
            | ([NULL], [NULL]) -> (match stack with 
                            | VCL(Num x2, e')::VCL(Num x1, e'')::stack' -> krivine (VCL(Num (sub x1 x2), e'')::stack') env pc' 
                            | _ -> raise BadStack)
            | ([NULL], _) -> krivine stack env (e2 @ [SUB([NULL], [NULL])] @ pc') 
            | (_, _) -> krivine stack env (e1 @ [SUB([NULL], e2)] @ pc'))
    (* Division *)
    | DVD(e1, e2) :: pc' ->   (match (e1, e2) with 
            | ([NULL], [NULL]) -> (match stack with 
                            | VCL(Num x2, e')::VCL(Num x1, e'')::stack' -> krivine (VCL(Num (div x1 x2), e'')::stack') env pc' 
                            | _ -> raise BadStack)
            | ([NULL], _) -> krivine stack env (e2 @ [DVD([NULL], [NULL])] @ pc') 
            | (_, _) -> krivine stack env (e1 @ [DVD([NULL], e2)] @ pc'))
    (* Multiplication *)
    | MUL(e1, e2) :: pc' ->   (match (e1, e2) with 
            | ([NULL], [NULL]) -> (match stack with 
                            | VCL(Num x2, e')::VCL(Num x1, e'')::stack' -> krivine (VCL(Num (mult x2 x1), e'')::stack') env pc' 
                            | _ -> raise BadStack)
            | ([NULL], _) -> krivine stack env (e2 @ [MUL([NULL], [NULL])] @ pc') 
            | (_, _) -> krivine stack env (e1 @ [MUL([NULL], e2)] @ pc'))
    (* Modulus *)
    | MOD(e1, e2) :: pc' ->   (match (e1, e2) with 
            | ([NULL], [NULL]) -> (match stack with 
                            | VCL(Num x2, e')::VCL(Num x1, e'')::stack' -> krivine (VCL(Num (rem x1 x2), e'')::stack') env pc' 
                            | _ -> raise BadStack)
            | ([NULL], _) -> krivine stack env (e2 @ [MOD([NULL], [NULL])] @ pc') 
            | (_, _) -> krivine stack env (e1 @ [MOD([NULL], e2)] @ pc'))
  (* Unary operations : Bool *)
    | NT(e) :: pc' -> (match e with
            | [NULL] -> (match stack with
                            | VCL(Bool b, e')::stack' -> krivine (VCL(Bool(not b), e')::stack') env pc' 
                            | _ -> raise BadStack)
            | _ -> krivine stack env (e @ [NT([NULL])] @ pc') )
  (* Binary operations : Bool *)
    (* Conjunction *)
    | AND(e1, e2) :: pc'  ->  (match (e1, e2) with 
            | ([NULL], [NULL]) -> (match stack with 
                            | VCL(Bool x2, e')::VCL(Bool x1, e'')::stack' -> krivine (VCL(Bool (x2 && x1), e'')::stack') env pc' 
                            | _ -> raise BadStack)
            | ([NULL], _) -> krivine stack env (e2 @ [AND([NULL], [NULL])] @ pc') 
            | (_, _) -> krivine stack env (e1 @ [AND([NULL], e2)] @ pc'))
    (* Disjunction *)
    | OR(e1, e2) :: pc'  ->  (match (e1, e2) with 
            | ([NULL], [NULL]) -> (match stack with 
                            | VCL(Bool x2, e')::VCL(Bool x1, e'')::stack' -> krivine (VCL(Bool (x2 || x1), e'')::stack') env pc' 
                            | _ -> raise BadStack)
            | ([NULL], _) -> krivine stack env (e2 @ [OR([NULL], [NULL])] @ pc') 
            | (_, _) -> krivine stack env (e1 @ [OR([NULL], e2)] @ pc'))
  (* Comparison Operations *)
    (* Basic CMP *)
    | CMPE(e) :: pc' -> (match e with
            | [NULL] -> (match stack with
                            | VCL(Num x, e')::stack' -> krivine (VCL(Bool (gt x (mk_big 0)), e')::stack') env pc' 
                            | _ -> raise BadStack)
            | _ -> krivine stack env (e @ [CMPE([NULL])] @ pc') )
    (* Equal *)
    | EQ :: pc' -> (match stack with 
            | VCL(Num x2, e')::VCL(Num x1, e'')::stack' -> krivine (VCL(Bool (eq x1 x2), e')::stack') env pc' 
            | _ -> raise BadStack)
    (* Greater Than *)
    | GTN :: pc' -> (match stack with 
            | VCL(Num x2, e')::VCL(Num x1, e'')::stack' -> krivine (VCL(Bool (gt x1 x2), e')::stack') env pc' 
            | _ -> raise BadStack)
    (* Less Than *)
    | LTN :: pc' -> (match stack with 
            | VCL(Num x2, e')::VCL(Num x1, e'')::stack' -> krivine (VCL(Bool (lt x1 x2), e')::stack') env pc' 
            | _ -> raise BadStack)
    (* Greater Than Equal *)
    | GTNE :: pc' -> (match stack with 
            | VCL(Num x2, e')::VCL(Num x1, e'')::stack' -> krivine (VCL(Bool (geq x1 x2), e')::stack') env pc' 
            | _ -> raise BadStack)
    (* Less Than Equal *)
    | LTNE :: pc' -> (match stack with 
            | VCL(Num x2, e')::VCL(Num x1, e'')::stack' -> krivine (VCL(Bool (leq x1 x2), e')::stack') env pc' 
            | _ -> raise BadStack)
  (* Conditional *)
    | CONDI(e2, e3) :: pc' -> (match stack with
            | VCL(Bool true, e')::stack' -> krivine stack' env (e2@pc')
            | VCL(Bool false, e'):: stack' -> krivine stack' env (e3@pc')
            | _ -> raise BadStack)
  (* Let condition *)
    | LETK(d, c) :: pc' -> (let x = fst (krivine stack env ([d]@c)) in krivine (x::stack) env pc')
  (* Function Abstraction *)
    | LAM(st, e1) :: pc' -> (krivine (FCL(st, e1, env)::stack) env  pc')
  (* Recursive Function Abstraction *)
    | RLAM(nm, st, e1) :: pc' -> (krivine (RFCL(nm, st, e1, env)::stack) ((nm, RFCL(nm, st, e1, env))::env) pc')
  (* Function Call *)
    | APP(e) :: pc' -> (match stack with 
                        | FCL(st, ex, env')::stack' -> let x = krivine stack' ((st, CL(e, env))::env') (ex) in krivine (fst(x)::stack') env pc'
                        | RFCL(nm, st, ex, env')::stack' -> let x = krivine stack' ((nm, RFCL(nm, st, ex, env'))::(st, CL(e, env))::env') ex in krivine (fst(x)::stack') env pc'
                        | _ -> raise BadStack)
  (* DEFINTIONS *)
    | DF(st, c) :: pc' -> (krivine stack ((st, CL(c, env))::env) pc')
