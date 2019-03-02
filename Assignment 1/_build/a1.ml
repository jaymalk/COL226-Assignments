
open A0

type  exptree =  N of int
              | Plus of exptree * exptree
              | Minus of exptree * exptree
              | Mult of exptree * exptree
              | Div of exptree * exptree
              | Rem of exptree * exptree
              | Nega of exptree
              | Abs of exptree

(* opcodes of the stack machine *)
type opcode = CONST of bigint | PLUS | TIMES | MINUS | DIV | REM | ABS | UNARYMINUS

(* Exceptions *)
exception IllformedStack;;


(* Definitional intepreter *)
(* Evaluation with inbuilt ocaml features *)
let eval (e:exptree) =
  let rec calc e = match e with
      N (x) -> x
    | Plus(e1, e2) -> (calc e1) + (calc e2)
    | Minus (e1, e2) -> (calc e1) - (calc e2)
    | Mult (e1, e2) -> (calc e1)*(calc e2)
    | Div (e1, e2) -> (calc e1)/(calc e2)
    | Rem (e1, e2) -> (calc e1) mod (calc e2)
    | Nega (e) -> - (calc e)
    | Abs (e) -> if (calc e) > 0 then (calc e) else - (calc e)
  in calc e
;;


(* The compile function, compiles the exptree into an opcode list. *)
let compile (e : exptree) =
  let rec mk_list  e = match e with
      N (x) -> [CONST(mk_big x)]
    | Plus(e1, e2) -> (mk_list e1) @ (mk_list e2) @ [PLUS]
    | Minus (e1, e2) -> (mk_list e1) @ (mk_list e2) @ [MINUS]
    | Mult (e1, e2) -> (mk_list e1) @ (mk_list e2) @ [TIMES]
    | Div (e1, e2) -> (mk_list e1) @ (mk_list e2) @ [DIV]
    | Rem (e1, e2) -> (mk_list e1) @ (mk_list e2) @ [REM]
    | Nega (e) -> (mk_list e) @ [UNARYMINUS]
    | Abs (e) -> (mk_list e) @ [ABS]
  in (mk_list e)
;;


(* Stack machine, evaluates the opcode list and puts the result on the top of stack (as list) *)
let rec stackmc (li : bigint list) (lo : opcode list) =
  let perform_action li oc = match oc with
      CONST (x)  -> x :: li
    | PLUS  -> (match li with
          x1 :: x2 :: xs -> (add x1 x2) :: xs
        | _ -> raise IllformedStack)
    | TIMES  -> (match li with
          x1 :: x2 :: xs -> (mult x1 x2) :: xs
        | _ -> raise IllformedStack)
    | MINUS  -> (match li with
          x1 :: x2 :: xs -> (sub x2 x1) :: xs
        | _ -> raise IllformedStack)
    | DIV  -> (match li with
          x1 :: x2 :: xs -> (div x2 x1) :: xs
        | _ -> raise IllformedStack)
    | REM  -> (match li with
          x1 :: x2 :: xs -> (rem x2 x1) :: xs
        | _ -> raise IllformedStack)
    | ABS  -> (match li with
          x :: xs -> (abs x) :: xs
        | _ -> raise IllformedStack)
    | UNARYMINUS  -> (match li with
          x :: xs -> (minus x) :: xs
        | _ -> raise IllformedStack)
  in
  let rec result li lo = match lo with
      [] -> List.hd li
    |x :: xs -> result (perform_action li x) xs
  in result li lo
;;
