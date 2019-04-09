open A1
exception Not_implemented
exception NotFound
exception Bad_State
exception Wrong_Type
exception General_Type

let rec item_from_list st g = match g with
    [] -> raise NotFound
  | x :: xs -> if fst x = st then snd x else item_from_list st xs
;;

let rec item_exists st g = match g with
    [] -> false
  | x :: xs -> if(fst x = st) then true else item_exists st xs
;;

(* whattype function to tell which is the type of exp *)
let rec gettype g e : exptype = match e with
  (* Basics *)
  | Var(st) -> (try item_from_list st g with NotFound -> raise NotFound)
  | N(x) -> (Tint)
  | B(b) -> (Tbool)
  (* Unary operations : Integers *)
  | Abs(e) ->      if (gettype g e = Tint) then (Tint) else raise Wrong_Type
  | Negative(e) -> if (gettype g e = Tint) then (Tint) else raise Wrong_Type
  (* Unary operations : Bool *)
  | Not(e) ->      if (gettype g e = Tbool) then (Tbool) else raise Wrong_Type
  (* Binary operations : Integers *)
  | Add(e1, e2)  -> if (gettype g e1 = Tint) && (gettype g e2 = Tint) then (Tint) else raise Wrong_Type
  | Sub(e1, e2)  -> if (gettype g e1 = Tint) && (gettype g e2 = Tint) then (Tint) else raise Wrong_Type
  | Mult(e1, e2) -> if (gettype g e1 = Tint) && (gettype g e2 = Tint) then (Tint) else raise Wrong_Type
  | Div(e1, e2)  -> if (gettype g e1 = Tint) && (gettype g e2 = Tint) then (Tint) else raise Wrong_Type
  | Rem(e1, e2)  -> if (gettype g e1 = Tint) && (gettype g e2 = Tint) then (Tint) else raise Wrong_Type
  (* Binary operations : Bool *)
  | Conjunction(e1, e2) -> if (gettype g e1 = Tbool) && (gettype g e2 = Tbool) then Tbool else raise Wrong_Type
  | Disjunction(e1, e2) -> if (gettype g e1 = Tbool) && (gettype g e2 = Tbool) then Tbool else raise Wrong_Type
  (* Comparison operations *)
  | Equals(e1, e2) ->    if (gettype g e1 = Tint) && (gettype g e2 = Tint) then (Tbool) else raise Wrong_Type
  | GreaterTE(e1, e2) -> if (gettype g e1 = Tint) && (gettype g e2 = Tint) then (Tbool) else raise Wrong_Type
  | LessTE(e1, e2) ->    if (gettype g e1 = Tint) && (gettype g e2 = Tint) then (Tbool) else raise Wrong_Type
  | GreaterT(e1, e2) ->  if (gettype g e1 = Tint) && (gettype g e2 = Tint) then (Tbool) else raise Wrong_Type
  | LessT(e1, e2) ->     if (gettype g e1 = Tint) && (gettype g e2 = Tint) then (Tbool) else raise Wrong_Type
  (* Parenthesis *)
  | InParen(e) -> (gettype g e)
  (* Conditional *)
  | IfThenElse(e1, e2, e3) -> if (gettype g e1 = Tbool) && ((gettype g e2)=(gettype g e3)) then (gettype g e2) else raise Wrong_Type
  (* Creating N-Tuple *)
  | Tuple(n, el) -> Ttuple(List.map (gettype g) el)
  (* Projecting a component of the tuple *)
  | Project((i, n), e) -> (match (gettype g e) with Ttuple(el) -> ( List.nth el (i-1) ) | _ -> raise Wrong_Type)
  (* Local definition *)
  | Let(df, e) -> (gettype ((yield g df) @ g) e)
  (* Functions *)
  | FunctionAbstractionType(st, tp, ex) -> Tfunc(tp, gettype ((st, tp)::g) ex)
  | FunctionCall(e1, e2) -> (match gettype g e1 with Tfunc(x, y) -> (match gettype g e2 with Tfunc(z, x) -> Tfunc(z, y) | x -> y) | _ -> raise Wrong_Type)
  (* All possible steps covered above, stage below should not be reached *)
  (* | _ -> (raise Bad_State) *)

(* get yield function to get G' *)
and yield g d = match d with
  | SimpleType(st, tp, ex) -> if gettype g ex = tp then [(st, tp)] else raise Wrong_Type
  | Sequence(dl) -> (let rec seq_yld g dl gl = match dl with [] -> List.rev gl | dd::ds -> seq_yld g ds ((yield (gl@g) dd) @ gl) in (seq_yld g dl []))
  | Parallel(dl) -> List.flatten (List.map (yield g) dl)
  | Local(dll, dl) -> yield ((yield g dll) @ g) dl
;;

(* hastype : ((string * exptype) list) -> exptree -> exptype -> bool *)
let rec hastype g e t = match e with
  (* Basics *)
  | Var(st) -> (List.mem (st, t) g)
  | N(x) -> (Tint = t)
  | B(b) -> (Tbool = t)
  (* Unary operations : Integers *)
  | Abs(e) ->      (hastype g e Tint) && (t = Tint)
  | Negative(e) -> (hastype g e Tint) && (t = Tint)
  (* Unary operations : Bool *)
  | Not(e) ->      (hastype g e Tbool) && (t = Tbool)
  (* Binary operations : Integers *)
  | Add(e1, e2)  -> (hastype g e1 Tint) && (hastype g e2 Tint) && (t = Tint)
  | Sub(e1, e2)  -> (hastype g e1 Tint) && (hastype g e2 Tint) && (t = Tint)
  | Mult(e1, e2) -> (hastype g e1 Tint) && (hastype g e2 Tint) && (t = Tint)
  | Div(e1, e2)  -> (hastype g e1 Tint) && (hastype g e2 Tint) && (t = Tint)
  | Rem(e1, e2)  -> (hastype g e1 Tint) && (hastype g e2 Tint) && (t = Tint)
  (* Binary operations : Bool *)
  | Conjunction(e1, e2) -> (hastype g e1 Tbool) && (hastype g e2 Tbool) && (t = Tbool)
  | Disjunction(e1, e2) -> (hastype g e1 Tbool) && (hastype g e2 Tbool) && (t = Tbool)
  (* Comparison operations *)
  | Equals(e1, e2) ->    (hastype g e1 Tint) && (hastype g e2 Tint) && (t = Tbool)
  | GreaterTE(e1, e2) -> (hastype g e1 Tint) && (hastype g e2 Tint) && (t = Tbool)
  | LessTE(e1, e2) ->    (hastype g e1 Tint) && (hastype g e2 Tint) && (t = Tbool)
  | GreaterT(e1, e2) ->  (hastype g e1 Tint) && (hastype g e2 Tint) && (t = Tbool)
  | LessT(e1, e2) ->     (hastype g e1 Tint) && (hastype g e2 Tint) && (t = Tbool)
  (* Parenthesis *)
  | InParen(e) -> (hastype g e t)
  (* Conditional *)
  | IfThenElse(e1, e2, e3) -> (gettype g e) = t
  (* Creating N-Tuple *)
  | Tuple(n, el) -> (match t with Ttuple(x) -> (List.for_all (fun x -> (x = true)) (List.map2 (hastype g) el x)) | _ -> false)
  (* Projecting a component of the tuple *)
  | Project((i, n), e1) -> (try let x = (gettype g e1) in (match x with Ttuple(el) -> (List.nth el (n-1) = t) | _ -> raise Wrong_Type) with Wrong_Type -> false)
  (* Local definition *)
  | Let(df, e) -> (try (hastype ((yield g df) @ g) e t) with _ -> false)
  (* Functions *)
  | FunctionAbstractionType(st, tp, ex) -> (match t with Tfunc(x, y) -> (tp = x && try ((gettype ((st, x)::g) ex) = y) with Wrong_Type-> false) | _ -> false)
  | FunctionCall(e1, e2) -> (try (let t1 = (gettype g e2) in (match e1 with FunctionAbstractionType(x, tp, y) -> if t1 = tp then hastype ((x, t1)::g) e1 t else false | Var(s) -> (item_from_list s g)=(Tfunc(t1, t)) | _ -> false)) with _ -> false)
  (* All possible steps covered above, stage below should not be reached *)
  (* | _ -> (raise Bad_State) *)


(* yields : ((string * exptype) list) -> definition -> ((string * exptype) list) -> bool *)
let rec yields g d g_dash =
    (yield g d) = g_dash
