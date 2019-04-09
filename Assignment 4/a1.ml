(* Dummy implementation of A1 *)
open A0
exception Not_implemented

(* Self defined exceptions *)
exception Bad_State (* To be raised when an undesired state has been reached *)
exception Confusing (* To be raised to keep ambiguous situations in check *)
exception IllformedStack (* To be raised when the opcodes leads to invalid opreations including stack *)
exception TupleSizeMismatch (* To be raised when the size of projection and tuple doesnt match *)

(* The possible types of expressions in the language of expressions *)
type exptype = Tint | Tunit | Tbool | Ttuple of (exptype list) | Tfunc of (exptype * exptype)

(* abstract syntax *)
type  exptree =
  Var of string (* variables starting with a Capital letter, represented as alphanumeric strings with underscores (_) and apostrophes (') *)
  | N of int      (* Integer constant *)
  | B of bool     (* Boolean constant *)
  (* unary operators on integers *)
  | Abs of exptree                   (* abs *)
  | Negative of exptree              (* unary minus ~ *)
  (* unary operators on booleans *)
  | Not of exptree
  (* binary operators on integers *)
  | Add of exptree * exptree         (* Addition + *)
  | Sub of exptree * exptree         (* Subtraction - *)
  | Mult of exptree * exptree        (* Multiplication * *)
  | Div of exptree * exptree         (* div *)
  | Rem of exptree * exptree         (* mod *)
  (* binary operators on booleans *)
  | Conjunction of exptree * exptree (* conjunction /\ *)
  | Disjunction of exptree * exptree (* binary operators on booleans \/ *)
  (* comparison operations on integers *)
  | Equals of exptree * exptree      (* = *)
  | GreaterTE of exptree * exptree   (* >= *)
  | LessTE of exptree * exptree      (* <= *)
  | GreaterT of exptree * exptree    (* > *)
  | LessT of exptree * exptree       (* < *)
  (* expressions using parenthesis *)
  | InParen of exptree               (* ( ) *)
  (* a conditional expression *)
  | IfThenElse of exptree * exptree * exptree (* if then else fi  *)
  (* creating n-tuples (n >= 0) *)
  | Tuple of int * (exptree list)
  (* projecting the i-th component of an expression (which evaluates to an n-tuple, and 1 <= i <= n) *)
  | Project of (int*int) * exptree   (* Proj((i,n), e)  0 < i <= n *)
  | Let of definition * exptree
  (* | FunctionAbstraction of string * exptree *)
  | FunctionCall of exptree * exptree
  (* New Function Abstraction (A4 extension) *)
  | FunctionAbstractionType of string * exptype * exptree
(* definition *)
and definition =
    (* Simple of string * exptree *)
  | SimpleType of string * exptype * exptree
  | Sequence of (definition list)
  | Parallel of (definition list)
  | Local of definition * definition
(* The language should contain the following types of expressions:  integers and booleans *)

(* The type of value returned by the definitional interpreter. *)
type value = NumVal of int | BoolVal of bool | TupVal of int * (value list)

(* DEFINITIONAL INTERPRETER *)
let eval (ex : exptree) (rho : string -> value) =
  let rec calc e = match e with
    (* Basics *)
    | Var(st) -> (rho st)
    | N(x) -> NumVal(x)
    | B(b) -> BoolVal(b)
    (* Unary operations : Integers *)
    | Abs(e) ->      (match (calc e) with | NumVal(bn) -> NumVal(if bn>0 then bn else -bn)   | _ -> raise Bad_State)
    | Negative(e) -> (match (calc e) with | NumVal(bn) -> NumVal(- bn) | _ -> raise Bad_State)
    (* Unary operations : Bool *)
    | Not(e) -> (match (calc e) with | BoolVal(b) -> BoolVal(not b) | _ -> raise Bad_State)
    (* Binary operations : Integers *)
    | Add(e1, e2)  -> (match ((calc e1), (calc e2)) with | (NumVal(x1), NumVal(x2)) -> NumVal(x1 + x2)   | _ -> raise Bad_State)
    | Sub(e1, e2)  -> (match ((calc e1), (calc e2)) with | (NumVal(x1), NumVal(x2)) -> NumVal(x1 - x2)   | _ -> raise Bad_State)
    | Mult(e1, e2) -> (match ((calc e1), (calc e2)) with | (NumVal(x1), NumVal(x2)) -> NumVal(x1 * x2)   | _ -> raise Bad_State)
    | Div(e1, e2)  -> (match ((calc e1), (calc e2)) with | (NumVal(x1), NumVal(x2)) -> NumVal(x1 / x2)   | _ -> raise Bad_State)
    | Rem(e1, e2)  -> (match ((calc e1), (calc e2)) with | (NumVal(x1), NumVal(x2)) -> NumVal(x1 mod x2) | _ -> raise Bad_State)
    (* Binary operations : Bool *)
    | Conjunction(e1, e2) -> (match ((calc e1), (calc e2)) with | (BoolVal(b1), BoolVal(b2)) -> BoolVal(b1 && b2) | _ -> raise Bad_State)
    | Disjunction(e1, e2) -> (match ((calc e1), (calc e2)) with | (BoolVal(b1), BoolVal(b2)) -> BoolVal(b1 || b2) | _ -> raise Bad_State)
    (* Comparison operations *)
    | Equals(e1, e2) ->    (match ((calc e1), (calc e2)) with | (NumVal(x1), NumVal(x2)) -> BoolVal(x1 == x2) | _ -> raise Bad_State)
    | GreaterTE(e1, e2) -> (match ((calc e1), (calc e2)) with | (NumVal(x1), NumVal(x2)) -> BoolVal(x1 >= x2) | _ -> raise Bad_State)
    | LessTE(e1, e2) ->    (match ((calc e1), (calc e2)) with | (NumVal(x1), NumVal(x2)) -> BoolVal(x1 <= x2) | _ -> raise Bad_State)
    | GreaterT(e1, e2) ->  (match ((calc e1), (calc e2)) with | (NumVal(x1), NumVal(x2)) -> BoolVal(x1 > x2)  | _ -> raise Bad_State)
    | LessT(e1, e2) ->     (match ((calc e1), (calc e2)) with | (NumVal(x1), NumVal(x2)) -> BoolVal(x1 < x2)  | _ -> raise Bad_State)
    (* Parenthesis *)
    | InParen(e) -> (calc e)
    (* Conditional *)
    | IfThenElse(e1, e2, e3) -> (match (calc e1) with |BoolVal(b) -> (if b then (calc e2) else (calc e3)) | _ -> raise Bad_State)
    (* Creating N-Tuple *)
    | Tuple(n, el) -> TupVal(n, List.map calc el)
    (* Projecting a component of the tuple *)
    | Project((i, n), e) -> (match (calc e) with | TupVal(m, al) -> if not (m == n) then (raise TupleSizeMismatch) else ((List.nth al (i-1))) | _ -> raise Bad_State)
    (* All possible steps covered above, stage below should not be reached *)
    | _ -> (raise Bad_State)
  in calc ex
;;

(* The type of value returned by the definitional interpreter. *)
type answer = Num of bigint | Bool of bool | Tup of int * (answer list)

(* opcodes of the stack machine (in the same sequence as above) *)
type opcode = VAR of string | NCONST of bigint | BCONST of bool | ABS | UNARYMINUS | NOT
  | PLUS | MINUS | MULT | DIV | REM | CONJ | DISJ | EQS | GTE | LTE | GT | LT
  | PAREN | IFTE | TUPLE of int | PROJ of int*int | LET | FABS | FCALL
            | SIMPLEDEF | SEQCOMPOSE | PARCOMPOSE | LOCALDEF

(* COMPILER *)
let compile (ex : exptree) =
  let rec mk_list e = match e with
    (* Basics *)
    | N(x) -> [NCONST(mk_big x)]
    | Var(x) -> [VAR(x)]
    | B(b) -> [BCONST(b)]
    (* Unary operations : Integers *)
    | Abs(x) ->      (mk_list x) @ [ABS]
    | Negative(x) -> (mk_list x) @ [UNARYMINUS]
    (* Unary operations : Bool *)
    | Not(e1) ->      (mk_list e1) @ [NOT]
    (* Binary operations : Integers *)
    | Add(e1, e2) ->   (mk_list e1) @ (mk_list e2) @ [PLUS]
    | Sub (e1, e2) ->  (mk_list e1) @ (mk_list e2) @ [MINUS]
    | Mult (e1, e2) -> (mk_list e1) @ (mk_list e2) @ [MULT]
    | Div (e1, e2) ->  (mk_list e1) @ (mk_list e2) @ [DIV]
    | Rem (e1, e2) ->  (mk_list e1) @ (mk_list e2) @ [REM]
    (* Binary operations : Bool *)
    | Conjunction(e1, e2) -> (mk_list e1) @ (mk_list e2) @ [CONJ]
    | Disjunction(e1, e2) -> (mk_list e1) @ (mk_list e2) @ [DISJ]
    (* Comparison operations *)
    | Equals(e1, e2) ->    (mk_list e1) @ (mk_list e2) @ [EQS]
    | GreaterTE(e1, e2) -> (mk_list e1) @ (mk_list e2) @ [GTE]
    | LessTE(e1, e2) ->    (mk_list e1) @ (mk_list e2) @ [LTE]
    | GreaterT(e1, e2) ->  (mk_list e1) @ (mk_list e2) @ [GT]
    | LessT(e1, e2) ->     (mk_list e1) @ (mk_list e2) @ [LT]
    (* Parenthesis *)
    | InParen(e) -> (mk_list e) @ [PAREN]
    (* Conditional *)
    | IfThenElse(e1, e2, e3) -> (mk_list e1) @ (mk_list e2) @ (mk_list e3) @ [IFTE]
    (* Handling N-Tuple *)
    | Tuple(n, el) -> (let rec opcode_list_from_tuple eli l m = match eli with
          [] -> l@[TUPLE(m)]
        | x :: xs -> opcode_list_from_tuple xs (l@(mk_list x)) m
       in opcode_list_from_tuple el [] n)
    (* Projecting a component of the tuple *)
    | Project((i, n), e) -> (mk_list e) @ [PROJ(i, n)]
    (* All possible steps covered above, stage below should not be reached *)
    | _ -> raise Bad_State
  in (mk_list ex)

let stackmc (stk : answer list) (rho : string -> answer) (pgm : opcode list) =
  (* Helper functions for processing each item in opcode list *)
  let perform_action li oc = match oc with
      NCONST(x) -> Num(x) :: li
    | BCONST(b) -> Bool(b) :: li
    | VAR(st)   -> (rho st) :: li
    (* Unary operations : Integers *)
    | ABS ->        (match List.hd li with Num(bn) -> Num(abs bn)::(List.tl li) | _ -> raise IllformedStack)
    | UNARYMINUS -> (match List.hd li with Num(bn) -> Num(minus bn)::(List.tl li) | _ -> raise IllformedStack)
    (* Unary operations : Bool *)
    | NOT -> (match List.hd li with Bool(b) -> Bool(not b)::(List.tl li) | _ -> raise IllformedStack)
    (* Binary operations : Integers *)
    | PLUS -> (match li with
          Num(x1) :: Num(x2) :: xs -> Num(add x2 x1) :: xs
        | _ -> raise IllformedStack)
    | MINUS -> (match li with
          Num(x1) :: Num(x2) :: xs -> Num(sub x2 x1) :: xs
        | _ -> raise IllformedStack)
    | MULT -> (match li with
          Num(x1) :: Num(x2) :: xs -> Num(mult x2 x1) :: xs
        | _ -> raise IllformedStack)
    | DIV -> (match li with
          Num(x1) :: Num(x2) :: xs -> Num(div x2 x1) :: xs
        | _ -> raise IllformedStack)
    | REM -> (match li with
          Num(x1) :: Num(x2) :: xs -> Num(rem x2 x1) :: xs
        | _ -> raise IllformedStack)
    (* Binary operations : Bool *)
    | CONJ -> (match li with
          Bool(b1) :: Bool(b2) :: xs -> Bool(b2 && b1) :: xs
        | _ -> raise IllformedStack)
    | DISJ -> (match li with
          Bool(b1) :: Bool(b2) :: xs -> Bool(b2 || b1) :: xs
        | _ -> raise IllformedStack)
    (* Comparison operations *)
    | EQS -> (match li with
          Num(x1) :: Num(x2) :: xs -> Bool(eq x2 x1) :: xs
        | _ -> raise IllformedStack)
    | GTE -> (match li with
          Num(x1) :: Num(x2) :: xs -> Bool(geq x2 x1) :: xs
        | _ -> raise IllformedStack)
    | LTE -> (match li with
          Num(x1) :: Num(x2) :: xs -> Bool(leq x2 x1) :: xs
        | _ -> raise IllformedStack)
    | LT -> (match li with
          Num(x1) :: Num(x2) :: xs -> Bool(lt x2 x1) :: xs
        | _ -> raise IllformedStack)
    | GT -> (match li with
          Num(x1) :: Num(x2) :: xs -> Bool(gt x2 x1) :: xs
        | _ -> raise IllformedStack)
    (* Parenthesis *)
    | PAREN -> li (* Tentatively, no change required after parenthesis (a redundant opcode) *)
    (* Conditional *)
    | IFTE -> (match li with
          a1 :: a2 :: Bool(b) :: xs -> if b then a2 :: xs else a1 :: xs
        | _ -> raise IllformedStack
      )
    (* N-Tuple related *)
    | TUPLE(n) -> (let rec take_out_and_put_back l1 l2 n_org m =
                     match m with
                       0 -> (Tup(n_org, l1)::l2)
                     | m -> (match l2 with
                           x :: xs -> (take_out_and_put_back (x::l1) xs n_org (m-1))
                         | _ -> raise Bad_State)
                   in take_out_and_put_back [] li n n)
    (* Projection related *)
    | PROJ(i, n) -> (match li with
          Tup(m, el) :: xs ->  if not (m==n) then raise TupleSizeMismatch else (List.nth el (i-1)) :: xs
        | _ -> raise IllformedStack
      )
    (* All possible steps covered above, stage below should not be reached *)
    | _ -> raise Bad_State
  in
  (* Main handle function *)
  let rec result li lo = match lo with
      [] -> List.hd li
    | x :: xs -> result (perform_action li x) xs
  in result stk pgm
