exception Bad_State
exception Procedure_Not_Found of string

type types = Tint | Tbool

type variable = Var of string * types

type procedure = Procedure of string * variable list * variable list * procedure list;;

type program = Main of variable list * procedure list;;

type structure = N of string * structure list | MAIN of structure list

let get_name (p : procedure) = match p with
  | Procedure(st, _, _, _) -> st
;;

let get_children (p : procedure) = match p with
  | Procedure(_, _, _, pl) -> pl
;;

let rec get_decendents (p : procedure) =
  (get_name p) :: (List.flatten (List.map get_decendents (get_children p)))
;;

let all_procedure (p : program) = match p with
  | Main(_, pl) -> (List.flatten (List.map get_decendents pl))
;;

let rec get_structure (p : procedure) =
  N(get_name p, List.map get_structure (get_children p) )
;;

let tree (p : program) = match p with
  | Main(_, pl) -> MAIN(List.map get_structure pl)
;;

let rec contains_procedure (nm : string) (tr : structure) = match tr with
  | MAIN (sl) -> List.fold_left (fun x y -> x || y) false (List.map (contains_procedure nm) sl)
  | N(n, []) -> n = nm
  | N(n, sl) -> if n = nm then true else List.fold_left (fun x y -> x || y) false (List.map (contains_procedure nm) sl)
;;

let rec find_procedure_list (nm : string) (tl : structure list) = match tl with
  | [] -> raise (Procedure_Not_Found nm)
  | t :: ts -> if contains_procedure nm t then t else find_procedure_list nm ts
;;

let rec get_all_ancestors (nm : string) (tr : structure) (al : string list) =
  try
    match tr with
    | MAIN (sl) -> (match (find_procedure_list nm sl) with
          N(n, sl') -> get_all_ancestors nm (N(n, sl')) (n::al)
        | _ -> raise Bad_State)
    | N(n, sl) -> (match (find_procedure_list nm sl) with
          N(n, sl') -> get_all_ancestors nm (N(n, sl')) (n::al)
        | _ -> raise Bad_State)
  with
  | Procedure_Not_Found nm -> al
;;
