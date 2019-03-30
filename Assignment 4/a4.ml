open A1
exception Not_implemented
exception NotFound
exception Bad_State

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
  | Var(st) -> item_from_list st g
  | N(x) -> (Tint)
  | B(b) -> (Tbool)
  (* Unary operations : Integers *)
  | Abs(e) ->      if (gettype g e = Tint) then (Tint) else raise Bad_State
  | Negative(e) -> if (gettype g e = Tint) then (Tint) else raise Bad_State
  (* Unary operations : Bool *)
  | Not(e) ->      if (gettype g e = Tbool) then (Tbool) else raise Bad_State
  (* Binary operations : Integers *)
  | Add(e1, e2)  -> if (gettype g e1 = Tint) && (gettype g e2 = Tint) then (Tint) else raise Bad_State
  | Sub(e1, e2)  -> if (gettype g e1 = Tint) && (gettype g e2 = Tint) then (Tint) else raise Bad_State
  | Mult(e1, e2) -> if (gettype g e1 = Tint) && (gettype g e2 = Tint) then (Tint) else raise Bad_State
  | Div(e1, e2)  -> if (gettype g e1 = Tint) && (gettype g e2 = Tint) then (Tint) else raise Bad_State
  | Rem(e1, e2)  -> if (gettype g e1 = Tint) && (gettype g e2 = Tint) then (Tint) else raise Bad_State
  (* Binary operations : Bool *)
  | Conjunction(e1, e2) -> if (gettype g e1 = Tbool) && (gettype g e2 = Tbool) then Tbool else raise Bad_State
  | Disjunction(e1, e2) -> if (gettype g e1 = Tbool) && (gettype g e2 = Tbool) then Tbool else raise Bad_State
  (* Comparison operations *)
  | Equals(e1, e2) ->    if (gettype g e1 = Tint) && (gettype g e2 = Tint) then (Tbool) else raise Bad_State
  | GreaterTE(e1, e2) -> if (gettype g e1 = Tint) && (gettype g e2 = Tint) then (Tbool) else raise Bad_State
  | LessTE(e1, e2) ->    if (gettype g e1 = Tint) && (gettype g e2 = Tint) then (Tbool) else raise Bad_State
  | GreaterT(e1, e2) ->  if (gettype g e1 = Tint) && (gettype g e2 = Tint) then (Tbool) else raise Bad_State
  | LessT(e1, e2) ->     if (gettype g e1 = Tint) && (gettype g e2 = Tint) then (Tbool) else raise Bad_State
  (* Parenthesis *)
  | InParen(e) -> (gettype g e)
  (* Conditional *)
  | IfThenElse(e1, e2, e3) -> if (gettype g e1 = Tbool) && (gettype g e2)=(gettype g e1) then (gettype g e2) else raise Bad_State
  (* Creating N-Tuple *)
  | Tuple(n, el) -> Ttuple(List.map (gettype g) el)
  (* Projecting a component of the tuple *)
  | Project((i, n), e) -> (match e with Tuple(n, el) -> ( gettype g (List.nth el (i-1))) | Var(x) -> (match item_from_list x g with Ttuple(l) -> List.nth l (i-1) | _ -> raise Bad_State) | _ -> raise Bad_State)
  (* Local definition *)
  (* | Let(df, e) -> (hastype (g @ (yield g d)) e t) *)
  (* Functions *)
  (* | FunctionAbstraction(st, ex) -> Tfunc(infer_type (st, )::g ex, infer_type g ex) *)
  | FunctionCall(e1, e2) -> (match gettype g e1 with Tfunc(x, y) -> (match gettype g e2 with Tfunc(z, x) -> Tfunc(z, y) | x -> y) | _ -> raise Bad_State)
  (* All possible steps covered above, stage below should not be reached *)
  | _ -> (raise Bad_State)
;;

(* get yield function to get G' *)
let rec yield g d = match d with
    Simple(st, ex) -> [(st, gettype g ex)]
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
  | IfThenElse(e1, e2, e3) -> (hastype g e1 Tbool) && (hastype g e2 t) && (hastype g e3 t)
  (* Creating N-Tuple *)
  | Tuple(n, el) -> (match t with Ttuple(x) -> (List.for_all (fun x -> (x = true)) (List.map2 (hastype g) el x)) | _ -> false)
  (* Projecting a component of the tuple *)
  | Project((i, n), e1) -> (gettype g e) = t
  (* Local definition *)
  | Let(df, e) -> (hastype ((yield g df) @ g) e t)
  (* Functions *)
  | FunctionAbstraction(st, ex) -> (match t with Tfunc(x, y) -> (gettype ((st, x)::g) ex) = y | _ -> false)
  | FunctionCall(e1, e2) -> let t1 = (gettype g e2) in (match e1 with FunctionAbstraction(x, y) -> hastype ((x, t1)::g) e1 t | Var(s) -> (item_from_list s g)=(Tfunc(t1, t)) | _ -> false)
  (* All possible steps covered above, stage below should not be reached *)
  (* | _ -> (raise Bad_State) *)


(* yields : ((string * exptype) list) -> definition -> ((string * exptype) list) -> bool *)
let rec yields g d g_dash =
    (yield g d) = g_dash
