open Bigint
exception Not_implemented

(* Self defined exceptions *)
exception Bad_State (* To be raised when an undesired state has been reached *)
exception Not_Found of string (* To be raised when a literal is not present in the table *)
exception TupleSizeMismatch (* To be raised when the size of projection and tuple doesnt match *)
exception Read_Definition (* To be raised when definition is to be parsed instead of expression *)
exception BadStack (* To be raised when the stack contents are not as desired *)

(* abstract syntax *)
type  exptree =
  V of string (* variables starting with a Capital letter, represented as alphanumeric strings with underscores (_) and apostrophes (') *)
  | Integer of bigint      (* Integer constant *)
  | Bool of bool     (* Boolean constant *)
  (* unary operators on integers *)
  | Abs of exptree                   (* abs *)
  | Negative of exptree              (* unary minus ~ *)
  (* unary operators on booleans *)
  | Not of exptree
  (* binary operators on integers *)
  | Plus of exptree * exptree        (* Addition + *)
  | Minus of exptree * exptree       (* Subtraction - *)
  | Mult of exptree * exptree        (* Multiplication * *)
  | Div of exptree * exptree         (* Division *)
  | Rem of exptree * exptree         (* Modulo *)
  (* binary operators on booleans *)
  | And of exptree * exptree         (* conjunction /\ *)
  | Or of exptree * exptree          (* binary operators on booleans \/ *)
  (* comparison operations on integers *)
  | Cmp of exptree
  | Equals of exptree * exptree      (* = *)
  | GreaterTE of exptree * exptree   (* >= *)
  | LessTE of exptree * exptree      (* <= *)
  | GreaterT of exptree * exptree    (* > *)
  | LessT of exptree * exptree       (* < *)
  (* expressions using parenthesis *)
  | InParen of exptree               (* ( ) *)
  (* a conditional expression *)
  | If_Then_Else of exptree * exptree * exptree (* if then else fi  *)
  (* creating n-tuples (n >= 0) *)
  | Tuple of int * (exptree list)
  (* projecting the i-th component of an expression (which evaluates to an n-tuple, and 1 <= i <= n) *)
  | Project of (int*int) * exptree   (* Proj((i,n), e)  0 < i <= n *)
  | Let of definition * exptree
  (* | FunctionAbstraction of string * exptree *)
  | App of exptree * exptree
  (* Recursive Function Abstraction *)
  | RecFuncAbs of string * string * exptree
  (* New Function Abstraction (A4 extension) *)
  | Lambda of string * exptree
  (* For additional handeling in interpreter *)
  | Watch
  | Null
  | Exit
(* definition *)
and definition =
    (* Simple of string * exptree *)
  | SimpleType of string * exptree
  | Sequence of (definition list)
  | Parallel of (definition list)
  | Local of definition * definition
  (* While Loop *)
  | While of exptree * definition
  ;;


(* ================================================ *)
(* SIMPLE EVALUATOR (DEFINITIONAL) *)
(* ================================================ *)

(* The type of value returned by the definitional interpreter. *)
type value = NumVal of bigint | BoolVal of bool | TupVal of int * (value list)
           | Func of (((string* value) list) * string * exptree) | RecFunc of (((string* value) list) * string * string * exptree)
           | NullVal | WatchVal | ExitVal

(* Functions related to tables *)
let rec find_in_table (table : (string * value) list) (s : string) = match table with
[] -> raise (Not_Found s)
| d :: ds -> if (fst d) = s then (d) else (find_in_table ds s)
;;

let rec find_mapping (table : (string * value) list) (s : string) =
  snd (find_in_table table s);;


(* DEFINITIONAL INTERPRETER *)
let rec eval (ex : exptree) (rho : (string * value) list) =
  let rec calc e = match e with
    (* Basics *)
    | V(st) -> ((find_mapping rho) st)
    | Integer(x) -> NumVal(x)
    | Bool(b) -> BoolVal(b)
    (* Unary operations : Integers *)
    | Abs(e) ->      (match (calc e) with | NumVal(bn) -> NumVal(abs bn)   | _ -> raise Bad_State)
    | Negative(e) -> (match (calc e) with | NumVal(bn) -> NumVal(minus bn) | _ -> raise Bad_State)
    (* Unary operations : Bool *)
    | Not(e) -> (match (calc e) with | BoolVal(b) -> BoolVal(not b) | _ -> raise Bad_State)
    (* Binary operations : Integers *)
    | Plus(e1, e2)  ->  (match ((calc e1), (calc e2)) with | (NumVal(x1), NumVal(x2)) -> NumVal(add x1 x2)  | _ -> raise Bad_State)
    | Minus(e1, e2)  -> (match ((calc e1), (calc e2)) with | (NumVal(x1), NumVal(x2)) -> NumVal(sub x1 x2)  | _ -> raise Bad_State)
    | Mult(e1, e2) ->   (match ((calc e1), (calc e2)) with | (NumVal(x1), NumVal(x2)) -> NumVal(mult x1 x2) | _ -> raise Bad_State)
    | Div(e1, e2)  ->   (match ((calc e1), (calc e2)) with | (NumVal(x1), NumVal(x2)) -> NumVal(div x1 x2)  | _ -> raise Bad_State)
    | Rem(e1, e2)  ->   (match ((calc e1), (calc e2)) with | (NumVal(x1), NumVal(x2)) -> NumVal(rem x1 x2)  | _ -> raise Bad_State)
    (* Binary operations : Bool *)
    | And(e1, e2) -> (match ((calc e1), (calc e2)) with | (BoolVal(b1), BoolVal(b2)) -> BoolVal(b1 && b2) | _ -> raise Bad_State)
    | Or(e1, e2) ->  (match ((calc e1), (calc e2)) with | (BoolVal(b1), BoolVal(b2)) -> BoolVal(b1 || b2) | _ -> raise Bad_State)
    (* Comparison operations *)
    | Cmp(e) -> (match (calc e) with | NumVal(x) -> BoolVal(gt x (mk_big 0)) | _ -> raise Bad_State)
    | Equals(e1, e2) ->    (match ((calc e1), (calc e2)) with | (NumVal(x1), NumVal(x2)) -> BoolVal(eq x1 x2)  | _ -> raise Bad_State)
    | GreaterTE(e1, e2) -> (match ((calc e1), (calc e2)) with | (NumVal(x1), NumVal(x2)) -> BoolVal(geq x1 x2) | _ -> raise Bad_State)
    | LessTE(e1, e2) ->    (match ((calc e1), (calc e2)) with | (NumVal(x1), NumVal(x2)) -> BoolVal(leq x1 x2) | _ -> raise Bad_State)
    | GreaterT(e1, e2) ->  (match ((calc e1), (calc e2)) with | (NumVal(x1), NumVal(x2)) -> BoolVal(gt x1 x2)  | _ -> raise Bad_State)
    | LessT(e1, e2) ->     (match ((calc e1), (calc e2)) with | (NumVal(x1), NumVal(x2)) -> BoolVal(lt x1 x2)  | _ -> raise Bad_State)
    (* Parenthesis *)
    | InParen(e) -> (calc e)
    (* Conditional *)
    | If_Then_Else(e1, e2, e3) -> (match (calc e1) with |BoolVal(b) -> (if b then (calc e2) else (calc e3)) | _ -> raise Bad_State)
    (* Creating N-Tuple *)
    | Tuple(n, el) -> TupVal(n, List.map calc el)
    (* Projecting a component of the tuple *)
    | Project((i, n), e) -> (match (calc e) with | TupVal(m, al) -> if not (m == n) then (raise TupleSizeMismatch) else ((List.nth al (i-1))) | _ -> raise Bad_State)
    (* Let statements in expressions *)
    | Let(d, e) -> (eval e ( (extension rho d) @ (rho)))
    (* Function Abstraction *)
    | Lambda(st, e1) -> Func(rho, st, e1)
    (* Recursive Function Abstraction *)
    | RecFuncAbs(nm, st, e1) -> RecFunc(rho, nm, st, e1)
    (* Function Call *)
    | App(e1, e2) -> (match (calc e1) with
          Func(tab, s, fex) -> (eval fex ((s, calc e2)::tab))
        | RecFunc(tab, nm, s, fex) -> (eval fex (List.sort_uniq compare ((nm, RecFunc(tab, nm, s, fex))::(s, calc e2)::tab)))
        | _ -> raise Bad_State)
    (* Handle commands *)
    | Null -> NullVal
    | Watch -> WatchVal
    | Exit -> ExitVal
    (* All possible steps covered above, stage below should not be reached *)
    (* | _ -> (raise Bad_State) *)
  in calc ex
(* Extension from definitions to rho *)
and extension (table : (string * value) list) (d : definition) = match d with
    | SimpleType(st, ex) -> [(st, eval ex table)]
    | Sequence(dl) -> (let rec seq_ex (tab_o) (dl) (tab_n) = match dl with
                [] -> tab_n
                | d :: ds -> seq_ex (tab_o) (ds) ((extension (tab_n@tab_o) d)@tab_n)
                in seq_ex table dl [])
    | Parallel(dl) -> (List.flatten (List.map (extension table) dl))
    | Local(d1, d2) -> extension ((extension table d1)@table) d2
    (* While Loop *)
    | While(e, d) -> (match (eval e table) with BoolVal true -> (extension ((extension table d)@table) d)| BoolVal false -> table| _ -> raise Bad_State)
;;