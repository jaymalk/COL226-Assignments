open Bigint
open Exptree


(* ================================================ *)
(* SECD MACHINE *)
(* ================================================ *)

(* Opcodes for compilation *)
type opcode = VAR of string | N of bigint | B of bool | ABS | UNARYMINUS | NOT
  | PLUS | MINUS | MULT | DIV | REM | CONJ | DISJ | CMP | EQS | GTE | LTE | GT | LT
  | PAREN | COND of (opcode list * opcode list) | TUPLE of int | PROJ of int*int 
  | LET of (opcode list) * (opcode list) 
  | CLOS of string * (opcode list) | RCLOS of string * string * (opcode list) | APP | RET | SET | DEFRET
  | DEF of string | SEQCOMPOSE of (opcode list) | PARCOMPOSE of (opcode list) | LOCALDEF of (opcode)*(opcode) | WHILE of (opcode list)*(opcode list)
  | EXIT | WATCH | NULL

(* OPCODE CONVERTER FOR SECD *)
let rec compile (ex : exptree) : opcode list = match ex with
    | Integer(x) -> [N(x)]
    | V(x) -> [VAR(x)]
    | Bool(b) -> [B(b)]
    (* Unary operations : Integers *)
    | Abs(x) ->      (compile x) @ [ABS]
    | Negative(x) -> (compile x) @ [UNARYMINUS]
    (* Unary operations : Bool *)
    | Not(e1) ->      (compile e1) @ [NOT]
    (* Binary operations : Integers *)
    | Plus(e1, e2)  ->   (compile e1) @ (compile e2) @ [PLUS]
    | Minus(e1, e2) ->   (compile e1) @ (compile e2) @ [MINUS]
    | Mult(e1, e2)  ->   (compile e1) @ (compile e2) @ [MULT]
    | Div(e1, e2)   ->   (compile e1) @ (compile e2) @ [DIV]
    | Rem(e1, e2)   ->   (compile e1) @ (compile e2) @ [REM]
    (* Binary operations : Bool *)
    | And(e1, e2) -> (compile e1) @ (compile e2) @ [CONJ]
    | Or(e1, e2) ->  (compile e1) @ (compile e2) @ [DISJ]
    (* Comparison operations *)
    | Cmp(e1) -> (compile e1) @ [CMP]
    | Equals(e1, e2) ->    (compile e1) @ (compile e2) @ [EQS]
    | GreaterTE(e1, e2) -> (compile e1) @ (compile e2) @ [GTE]
    | LessTE(e1, e2) ->    (compile e1) @ (compile e2) @ [LTE]
    | GreaterT(e1, e2) ->  (compile e1) @ (compile e2) @ [GT]
    | LessT(e1, e2) ->     (compile e1) @ (compile e2) @ [LT]
    (* Parenthesis *)
    | InParen(e) -> (compile e) @ [PAREN]
    (* Conditional *)
    | If_Then_Else(e1, e2, e3) -> (compile e1) @ [COND((compile e2) , (compile e3))]
    (* Handling N-Tuple *)
    | Tuple(n, el) -> (let rec opcode_list_from_tuple eli l m = match eli with
          [] -> l@[TUPLE(m)]
        | x :: xs -> opcode_list_from_tuple xs (l@(compile x)) m
       in opcode_list_from_tuple el [] n)
    (* Projecting a component of the tuple *)
    | Project((i, n), e) -> (compile e) @ [PROJ(i, n)]
    (* Let statements in expressions *)
    | Let(d, e) -> [LET(def_compile d, compile(e)@[RET])]
    (* Function Abstraction *)
    | Lambda(st, e1) -> [CLOS( st, compile(e1)@[RET])]
    (* Recursive Function Abstraction *)
    | RecFuncAbs(nm, st, e1) -> [RCLOS(nm, st, compile(e1)@[RET])]
    (* Function Call *)
    | App(e1, e2) -> (compile e1) @ (compile e2) @ [APP]
    (* Handle commands *)
    | Null -> [NULL]
    | Watch -> [WATCH]
    | Exit -> [EXIT]
(* OPCODE CONVERTER IN DEFINITIONS *)
and def_compile (df : definition) : opcode list = match df with
    | SimpleType(st, ex) -> ((compile ex) @ [DEF(st)])
    | Sequence(dl) -> (match dl with [] -> [] | d :: ds -> (def_compile d)@(def_compile (Sequence(ds))))
    | While(e, d) -> [WHILE(compile e, def_compile d)]
    | _ -> print_string("Definition not implemented"); raise Not_implemented    
    (* | Parallel(dl) -> PARCOMPOSE(List.map def_compile dl) *)
    (* | Local(d1, d2) -> LOCALDEF(def_compile d1, def_compile d2) *)  

(* ------------------------------------------------------------------------ *)
(* ANSWER, GAMMA and CLOSURE TYPES | FOR SECD MACHINE *)
type answer = Num of bigint | Bool of bool | Tup of int * (answer list)
and gamma = (string * closure) list
and closure = VCL of (answer * gamma)
            | FunCL of (string * (opcode list) * gamma) 
            | RFunCL of (string * string * (opcode list) * gamma)
(* ------------------------------------------------------------------------ *)

(* Function for accessing gamma elements *)
let rec get ((st, gm) : (string * gamma)) = match gm with
[] -> raise (Not_Found(st));
| x :: xs -> if (fst x)=st then (snd x) else get(st, xs)
;; 

(* SECD Operation *)
let rec secd (stack : closure list) (env : gamma) (code : opcode list) (dump : ((closure list)*(gamma)*(opcode list)) list) = match (stack, code) with
(* Evaluation Complete *)
| (stack, []) -> (stack, env)
(* User level opcodes, for interpreter *)
| (stack , EXIT::code') -> exit 0
| (stack , NULL::code') -> (secd stack env code' dump)
(* Basic Value closures *)
| (_, N(x)::code') ->    secd (VCL(Num(x), [])::stack) env code' dump
| (_, VAR(st)::code') -> secd (get(st, env)::stack) env code' dump
| (_, B(b)::code') ->    secd (VCL(Bool(b), [])::stack) env code' dump
(*---------- EXPRESSIONS ----------*)
(* Unary operations : Integers *)
| (VCL(Num(x), e')::stack', ABS::code') ->  secd (VCL(Num(abs x), e')::stack') env code' dump
| (VCL(Num(x), e')::stack', UNARYMINUS::code') ->  secd (VCL(Num(minus x), e')::stack') env code' dump
(* Unary operations : Bool *)
| (VCL(Bool(b), e')::stack', NOT::code') ->  secd (VCL(Bool(not b), e')::stack') env code' dump
(* Binary operations : Integers *)
| (VCL(Num(x1), e')::VCL(Num(x2), e'')::stack', PLUS::code') ->   secd (VCL(Num(add x1 x2), e') ::stack') env code' dump
| (VCL(Num(x1), e')::VCL(Num(x2), e'')::stack', MINUS::code') ->  secd (VCL(Num(sub x2 x1), e') ::stack') env code' dump
| (VCL(Num(x1), e')::VCL(Num(x2), e'')::stack', MULT::code') ->   secd (VCL(Num(mult x1 x2), e')::stack') env code' dump
| (VCL(Num(x1), e')::VCL(Num(x2), e'')::stack', DIV::code') ->    secd (VCL(Num(div x2 x1), e') ::stack') env code' dump
| (VCL(Num(x1), e')::VCL(Num(x2), e'')::stack', REM::code') ->    secd (VCL(Num(rem x2 x1), e') ::stack') env code' dump
(* Binary operations : Bool *)
| (VCL(Bool(b1), e')::VCL(Bool(b2), e'')::stack', CONJ::code') ->  secd (VCL(Bool(b1 && b2), e')::stack') env code' dump
| (VCL(Bool(b1), e')::VCL(Bool(b2), e'')::stack', DISJ::code') ->  secd (VCL(Bool(b1 || b2), e')::stack') env code' dump
(* Comparison operations *)
| (VCL(Num(x), e')::stack', CMP::code') ->  secd (VCL(Bool(gt x (mk_big 0)), e')::stack') env code' dump
| (VCL(Num(x1), e')::VCL(Num(x2), e'')::stack', EQS::code') ->  secd (VCL(Bool(eq x1 x2), e') ::stack') env code' dump
| (VCL(Num(x1), e')::VCL(Num(x2), e'')::stack', GTE::code') ->  secd (VCL(Bool(geq x2 x1), e')::stack') env code' dump
| (VCL(Num(x1), e')::VCL(Num(x2), e'')::stack', LTE::code') ->  secd (VCL(Bool(leq x2 x1), e')::stack') env code' dump
| (VCL(Num(x1), e')::VCL(Num(x2), e'')::stack', GT::code') ->   secd (VCL(Bool(gt x2 x1), e') ::stack') env code' dump
| (VCL(Num(x1), e')::VCL(Num(x2), e'')::stack', LT::code') ->   secd (VCL(Bool(lt x2 x1), e') ::stack') env code' dump
(* Parenthesis *)
| (st, PAREN::code') -> secd st env code' dump
(* Conditional *)
| (VCL(Bool(b), e')::stack', COND(c1, c2)::code') ->  if b then 
            secd stack' env (c1@code') dump else secd stack' env (c2@code') dump
(* N-tuples *)
| (stack, TUPLE(m)::code') -> (let rec take_out_and_put_back l1 l2 n_org nn = match nn with
  0 -> (VCL(Tup(n_org, l1), [])::l2)
| nn -> (match l2 with
      VCL(x, _) :: xs -> (take_out_and_put_back (x::l1) xs n_org (nn-1))
    | _ -> print_string("Tuple not possible\n");raise Bad_State)
in secd (take_out_and_put_back [] stack m m) env code' dump)
(* Projection *)
| (VCL(Tup(n, al), e')::stack', PROJ(i, m)::code') -> if m=n then secd ((VCL(List.nth al (i-1), e'))::stack') env code' dump else raise TupleSizeMismatch
(* Let statements *)
| (stack, LET(c1, c2)::code') -> secd [] env (c1@c2) ((stack, env, code') :: dump)
(* Function Abstraction *)
| (stack', CLOS(st, c')::code') -> secd (FunCL(st, c', env)::stack') env code' dump
(* Recursive Function Abstraction *)
| (stack', RCLOS(nm, st, c')::code') -> secd (RFunCL(nm, st, c', env)::stack') env code' dump
(* Function Call *)
| (VCL(a1, e')::FunCL(st, c1, e'')::stack', APP::code') -> secd [] ((st, VCL(a1, e'))::e'') c1 ((stack', env, code') :: dump)
(* Recursive Function Call *)
| (VCL(a1, e')::RFunCL(nm, st, c1, e'')::stack', APP::code') -> secd [] ((st, VCL(a1, e'))::(nm, RFunCL(nm, st, c1, e''))::e'') c1 ((stack', env, code') :: dump)
(* RETURN CODE *)
| (vcl :: stack'', RET::code'') -> (match dump with [] -> raise Bad_State | (s, ev, cd)::xs -> secd (vcl::s) ev cd xs)
(*------------ DEFINITIONS ------------*)
(* Simple *)
| (vcl::stack', DEF(st)::code') -> secd stack' ((st, vcl)::env) code' (dump)
(* Sequential and Parallel (NOT SET) *)
(* Local (NOT SET) *)
(* While loop *)
| (stack', WHILE(ce, cd)::code') -> secd stack' env (ce@[COND(cd@[WHILE(ce, cd)], [NULL])]@code') (dump)
(* Not implemented exception *)
| _ -> print_string("Problem : Bad State\n"); raise Bad_State
