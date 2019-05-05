open Lexer
open Parser
open Procedure

exception Variable_Error of string

(* GLOBAL VARIABLES *)
(* =================================== *)
let (program : procedure ref) = ref (Procedure("Empty", [], [], []));;
let (s : stack ref) = ref [];;
let (fp : int ref) = ref (-4);;
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

(* VARIABLE ACCESS MECHANISM *)
(* =================================== *)
(* Checking if varaible is in the present list *)
let rec in_list (ol : opcode list) (v : string) = match ol with
[] -> false
| (MAP(x, y) :: ol') -> if x = v then true else in_list ol' v
| _ -> raise (Bad_State "[in_list] List should only contain MAPs\n")
;;
(* Finding the variable in the list *)
let rec find_in_list (ol : opcode list) (v : string) = match ol with
(MAP(x, y) :: ol') -> if x = v then y else find_in_list ol' v
| _ -> raise (Bad_State "[find_in_list] List should only contain MAPs\n")
;;
(* Resetting the list and then adjusting the stack *)
let reset_list (ol : opcode list) (v : string) (f : int) (nv : int) (b : bool) = 
  let rec list_set ol oln v nv = match ol with 
  [] -> List.rev oln
| (MAP(x, y) :: ol') -> if x = v then list_set ol' (MAP(x, nv) :: oln) v nv else list_set ol' (MAP(x, y) :: oln) v nv
| _ -> raise (Bad_State "[reset_list] List should only contain MAPs\n")
  in let new_pos = if b then LOCAL(list_set ol [] v nv) else PASSED(list_set ol [] v nv)
      in s := (List.mapi (fun i x -> if i = f then new_pos else x) !s)
;;

(* Finding the char in the stack *)
let rec find_char (v : string) (f : int)  = 
  if f <= 0 then raise (Variable_Error "Variable not found/accessible\n") else
  match (List.nth !s f) with
| LOCAL(ol) ->  if in_list ol v then find_in_list ol v else find_char v (f-3)
| PASSED(ol) -> if in_list ol v then find_in_list ol v else let x = (fun (STATIC(z)) -> z) (List.nth !s (f+2)) 
                                  in if x = 1 then raise (Variable_Error "Variable not found/accessible")
                                    else find_char v (x+2)
| _ -> raise (Bad_State "[find_char] Invalid access point. (Match-case)\n")
;;
(* Finding the char in the stack and then resetting its value *)
let rec find_and_reset_char (v : string) (f : int) (nv : int) = 
  if f <= 0 then raise (Variable_Error "Variable not found/accessible\n") else
  match (List.nth !s f) with
| LOCAL(ol) ->  if in_list ol v then reset_list ol v f nv true else find_and_reset_char v (f-3) nv
| PASSED(ol) -> if in_list ol v then reset_list ol v f nv false else let x = (fun (STATIC(z)) -> z) (List.nth !s (f+2)) 
                                  in if x = 1 then raise (Variable_Error "Variable not found/accessible")
                                    else find_and_reset_char v (x+2) nv
| _ -> raise (Bad_State "[find_char] Invalid access point. (Match-case)\n")
;;
(* Accessing function (public) *)
let access x = find_char x (!fp+2)
;;
(* Reset Value *)
let reset x nv = match nv with
  N(n) -> find_and_reset_char x (!fp+2) n; n
| V(x) -> find_and_reset_char x (!fp+2) (access(x)); access(x)
;;

(* SETTING MECHANISM *)
(* =================================== *)
(* Assigning variables their values *)
let rec assign (v : intype list) (pl : opcode list) (new_list : opcode list)= match (v, pl) with
([], []) -> List.rev new_list
| (N(n)::vs, MAP(st, _)::ps) -> assign vs ps (MAP(st, n)::new_list)
| (V(x)::vs, MAP(st, _)::ps) -> (try assign vs ps (MAP(st, access(x))::new_list) with (Variable_Error(s)) as e -> raise e | _ as e-> raise e)
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
;;
(* Calling a procedure on interpreter *)
let rec call (cl : call) = match cl with
  | Call(st, var_list) -> let x = List.length !s in process_set x (List.assoc st !frames) var_list
  | _ -> raise (Bad_State "[call] Invalid Call.")
;;

(* PRINTING FORMAT *)
(* =================================== *)
(* Printing an opcode *)
let rec print_opcode oc = match oc with
 STATIC(n) -> print_string("\027[1;35mStatic Link : "^string_of_int(n)^"\n\027[0m")
 | LOCAL(ol) -> print_string("\027[1;31mLocal Variables : \027[0m"); print_list(ol)
 | PASSED(ol) -> print_string("\027[1;31mPassed Varaibles : \027[0m"); print_list(ol)
 | START(st, n) -> print_string("\027[1;36mFrame Start : "^st^", with FP = "^string_of_int(n)^"\n\027[0m")
 | MAP(st, n) -> print_string("\027[1;33m("^st^", "^string_of_int(n)^") \027[0m")
and print_list ol = match ol with
| [] -> print_string("\n")
| o :: ol' -> print_opcode o ; print_list ol'
 ;;

(* Priting stack contents *)
let rec print_stack_contents (s : stack) = match s with
  [] -> print_string("\n\027[0m")
  | START(_, _) :: s' -> print_opcode(List.hd s); print_string("\027[1;32m========================\n\027[0m"); print_stack_contents(s')
  | head :: s' -> print_opcode(head); print_stack_contents(s')
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
      (let command = (Parser.dynamic_parser Lexer.read (lexbuf)) in 
      match command with
      | Call(st, vl) -> if is_valid st then (s := !s @ (call command) ; fp := !fp + 4) else print_string("Not Callable...\n")
      | Access (st) -> (try let x = access(st) in print_string("\027[1;33mVariable "^st^": "^string_of_int(x)^"\n\027[0m") with (Variable_Error s) -> print_string(s)) 
      | Set (st, nv) -> (try let x = reset st nv in print_string("\027[1;33mVariable Set "^st^": "^string_of_int(x)^"\n\027[0m") with (Variable_Error s) -> print_string(s)) 
      | Stack_Trace -> (* Printing stack trace *)
                print_string("\n\027[1;32m========================\n\027[0m");print_stack_contents(List.rev !s);
      );
    with
      | Parsing.Parse_error -> print_string("Parsing Error. Please Enter Again!\n"); Lexing.new_line lexbuf;
      | _ as e -> raise e
  done
;;