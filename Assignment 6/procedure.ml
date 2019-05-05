exception Not_Implemented of string
exception Error of string
exception Variable_Count_Mismatch
exception Bad_State of string
exception Procedure_Not_Found of string

type types = Tint

type variable = Var of string * types

type procedure = Procedure of string * variable list * variable list * procedure list;;

type gamma = (string * int) list;;

type call = Call of string * intype list 
and intype = N of int | V of string;;

type opcode =
   (* Stack opcodes *)
  STATIC of int | LOCAL of opcode list | PASSED of opcode list | START of string * int | RET of int
   (* String opcode *)
  | MAP of string * int
  and
frame = opcode list;;

(* Hardcoded frames linked to their names *)
type frame_sets = (string * frame) list;;

type stack = opcode list;;

(* GLOBAL VARIABLES *)
let (program : procedure ref) = ref (Procedure("Empty", [], [], []));;
let (s : stack ref) = ref [];;
let (fp : int ref) = ref 0;;
let (frames : frame_sets ref) =ref [];;

(* Compiling varialbles *)
let compile oc = match oc with
| Var(st, _) -> MAP(st, 0)
;;

(* Creating hardcoded frames from the given code body *)
let rec hardcode_frames (fs : frame_sets) (p : procedure) : frame_sets = match p with
  | Procedure("Main", passed_vl, local_vl, pl) -> let y = ("Main", (START ("Main", 0) :: STATIC (0) :: (LOCAL (List.map compile local_vl)) :: [])) in map_to_list (y::fs) pl
  | Procedure(st, passed_vl, local_vl, pl) -> let y = (st, (PASSED (List.map compile passed_vl)) :: START (st, 0) :: STATIC (0) :: (LOCAL (List.map compile local_vl)) :: []) in map_to_list (y::fs) pl
and map_to_list fs pl = match pl with
    [] -> fs
    | p :: ps -> map_to_list (hardcode_frames fs p) ps 
;;

(* Assigning variables their values *)
let rec assign (v : intype list) (pl : opcode list) (new_list : opcode list)= match (v, pl) with
([], []) -> List.rev new_list
| (N(n)::vs, MAP(st, _)::ps) -> assign vs ps (MAP(st, n)::new_list)
| (V(x)::vs, MAP(st, _)::ps) -> raise (Not_Implemented "Varaible Fetch Not Implemented")
| _ -> raise Variable_Count_Mismatch
;;

(* Finding the parent name, given the name of the procedure and complete map *)
let rec contains p proc = match proc with
  | Procedure(st, _, _, pl) -> if st = p then true else (List.fold_left (fun x y -> x || y) false (List.map (contains p) pl))
  ;;

let get_parent_name p =
  let rec parent p prnt curr = match curr with
  | Procedure(st, _, _, pl) -> if st = p then (fun (Procedure(st, _, _, _)) -> st) prnt else
    let rec apply_on_list p curr pl = (match pl with
    [] -> raise (Error "[get_parent_name] : Procedure not found.")
    | pp :: ps -> if (contains p pp) then (parent p curr pp) else apply_on_list p curr ps)
    in apply_on_list p curr pl 
  in parent p !program !program
;;
    

(* Finding the parent index *)
let rec get_parent_index (p : string) = let y = (get_parent_name p) in 
  match (List.find (fun x -> (match x with START(z, _)-> z=y | _-> false)) (List.rev !s)) with
  START(y, indx) -> indx
  | _ -> raise (Bad_State "[get_parent_index]")
;;


(* Setting the set according to an external call *)
let rec process_set (start : int) (f : frame) (v : intype list) = match f with
| PASSED(pl) :: START(name, 0) :: STATIC(0) :: ls -> PASSED(assign v pl []) :: START(name, start+1) :: STATIC(get_parent_index name) :: ls
| START("Main", 0) :: STATIC(0) :: ls  -> if not (start = 0) then raise (Error "Main can only be called in the start.") else START("Main", 0) :: STATIC(1) ::  ls
| _ -> raise (Bad_State "[process_set] : No other possibility of frame.")
;;


(* Calling a procedure on interpreter *)
let rec call (cl : call) = match cl with
  | Call(st, var_list) -> let x = List.length !s in process_set x (List.assoc st !frames) var_list
;;
