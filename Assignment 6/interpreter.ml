open Lexer
open Parser
open Procedure

(* GLOBAL VARIABLES *)
(* =================================== *)
let (program : procedure ref) = ref (Procedure("Empty", [], [], []));;
let (s : stack ref) = ref [];;
let (fp : int ref) = ref 0;;
let (frames : frame_sets ref) =ref [];;

(* FRAME DESIGNING *)
(* =================================== *)
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


(* PARENT FINDING IN TREE *)
(* =================================== *)
(* Checking if a procedure is contained in a subtree *)
let rec contains p proc = match proc with
  | Procedure(st, _, _, pl) -> if st = p then true else (List.fold_left (fun x y -> x || y) false (List.map (contains p) pl))
  ;;
(* Finding the parent name, given the name of the procedure and complete map *)
let get_parent_name (p : string) =
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
(* Getting the list of ancestors *)
let get_ancestor_list (p : string) =
  let rec get_list (pn : string) (pl : string list) = 
    if (List.mem "Main" pl) then pl 
    else let pq = get_parent_name pn in get_list (pq) (pn::pl)
  in get_list p [p]
;;
(* SETTING MECHANISM *)
(* =================================== *)
(* Assigning variables their values *)
let rec assign (v : intype list) (pl : opcode list) (new_list : opcode list)= match (v, pl) with
([], []) -> List.rev new_list
| (N(n)::vs, MAP(st, _)::ps) -> assign vs ps (MAP(st, n)::new_list)
| (V(x)::vs, MAP(st, _)::ps) -> raise (Not_Implemented "Varaible Fetch Not Implemented")
| _ -> raise Variable_Count_Mismatch
;;
(* Setting the set according to an external call *)
let rec process_set (start : int) (f : frame) (v : intype list) = match f with
| PASSED(pl) :: START(name, 0) :: STATIC(0) :: ls -> PASSED(assign v pl []) :: START(name, start+1) :: STATIC(get_parent_index name) :: ls
| START("Main", 0) :: STATIC(0) :: ls  -> if not (start = 0) then raise (Error "Main can only be called in the start.") else START("Main", 0) :: STATIC(1) ::  ls
| _ -> raise (Bad_State "[process_set] : No other possibility of frame.")
;;

(* CALLING MECHANISM *)
(* =================================== *)
(* Checking the validity of call *)
let rec is_valid (st : string) = 
  if st = "Main" then true else
  let x = (List.nth !s !fp)
    in List.mem (get_parent_name st) (get_ancestor_list ((fun (START(st, _)) -> st) x))
  
(* Calling a procedure on interpreter *)
let rec call (cl : call) = match cl with
  | Call(st, var_list) -> let x = List.length !s in process_set x (List.assoc st !frames) var_list
;;

(* PRINTING FORMAT *)
(* =================================== *)
(* Printing an opcode *)
let rec print_opcode oc = match oc with
 STATIC(n) -> print_string("Static Link : "^string_of_int(n)^"\n")
 | LOCAL(ol) -> print_string("Local Variables\n")
 | PASSED(ol) -> print_string("Passed Varaibles\n")
 | START(st, n) -> print_string("Frame Start : "^st^", with FP = "^string_of_int(n)^"\n")
 | MAP(st, n) -> print_string("( "^st^", "^string_of_int(n)^")")
 ;;

(* Priting stack contents *)
let rec print_stack_contents (s : stack) = match s with
  [] -> print_string("========================\n")
  | head :: s' -> print_opcode(head);  print_string("------------------------\n"); print_stack_contents(s')
;;

(* INTERPRETER *)
(* =================================== *)
let _ = 
  (* Reading the procedure from the file *)
  (let file = open_in "./example.txt" in
    program := procedure_parser Lexer.read (Lexing.from_channel file));
    frames := (hardcode_frames [] !program);
  (* Starting the interpreter *)
  print_string("Start with calling main.\n"); flush stdout;
  let lexbuf = Lexing.from_channel stdin in
  while true do
    try
      print_string("Enter Command : "); flush stdout;
      let command = (Parser.dynamic_parser Lexer.read (lexbuf)) in 
      match command with
      | Call(st, vl) -> if is_valid st then (s := !s @ (call command) ; fp := !fp + 4) else print_string("Not Callable...");
      print_string("========================\n");
      print_stack_contents(List.rev !s);
    with
      | Parsing.Parse_error -> print_string("Parsing Error. Please Enter Again!\n")
      | _ as e -> raise e
  done
;;