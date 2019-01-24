(* Assignment 0 *)


(* ---------------------------------------------- *)
(* Declaring the types *)
type sign = Neg | NonNeg ;;
type bigint = sign * int list;;

(* Product of signs *)
let sp (s1 : sign) (s2 : sign) = match (s1 == s2) with
    true -> NonNeg
  | false -> Neg;;

(* Constructor of bigint *)
let bigintmake (s : sign) (l : int list) : bigint = (s, l);;

(* Helper functions for Negation (int and int list) *)
let ( - ) (x : int) = -x;;  (* Simple int-negation *)
let minus_list (l : int list) = List.map ( - ) l;; (* Negate all ints in a list *)



(* Exceptions *)


(* ---------------------------------------------- *)
(* ARITHMETIC OPERATIONS *)

(* Helper functions for Addition and Subtraction *)
let propogate_carry (l : int list) =
  let rec spread_constant c l' = match (c >= 10) with
      true -> spread_constant (c/10) ((c mod 10)::l')
    | false -> c::l'
  in
  let rec rev_propogate l l' c = match l with
      [] -> if c == 0 then l' else spread_constant c l'
    | hd :: tl -> if hd+c < 0 then rev_propogate tl ((hd+c+10) mod 10::l') (-1)
      else rev_propogate tl (((hd+c) mod 10)::l') ((hd+c)/10)
  in rev_propogate (List.rev l) [] 0;;

let rec remove_zeros (l1 : int list) =  match l1 with
    [] -> []
  | x :: xs -> if x != 0 then l1 else (remove_zeros xs);;

let simple_add (l1 : int list) (l2 : int list) =
  let rec add_till_end l1 l2 la =
    if l1 == [] && l2 == [] then remove_zeros(propogate_carry(la))
    else if l1 == [] then (add_till_end [] (List.tl l2) ((List.hd l2) :: la))
    else if l2 == [] then (add_till_end (List.tl l1) [] ((List.hd l1) :: la))
    else add_till_end (List.tl l1) (List.tl l2) ((List.hd l1 + List.hd l2) :: la)
  in add_till_end (List.rev l1) (List.rev l2) [];;

let simple_subtract (l1 : int list) (l2 : int list) : bigint =
  let rec larger_list l1 l2 c = match (l1, l2) with
      (x:: xs, y::ys) -> larger_list xs ys (c&&(x>=y))
    | ([], y::ys) -> false
    | (x::xs, []) -> true
    | ([], []) -> c
  in match (larger_list l1 l2 true) with
    true -> (NonNeg, simple_add l1 (minus_list l2))
  | false ->(Neg, simple_add l2 (minus_list l1));;

(* Addition *)
let add (a : bigint) (b : bigint) = match (fst a, fst b) with
    (Neg, Neg) -> (Neg, simple_add (snd a) (snd b))
  | (NonNeg, NonNeg) -> (NonNeg, simple_add (snd a) (snd b))
  | (Neg, NonNeg) -> simple_subtract (snd b) (snd a)
  | (NonNeg, Neg) -> simple_subtract (snd a) (snd b);;

(* Unary Negation *)
let minus (a: bigint) = if (fst a) = NonNeg then bigintmake (Neg) (snd a)
  else bigintmake (NonNeg) (snd a);;

(* Subtraction *)
let sub (a: bigint) (b: bigint) = add (a) (minus b);;

(* Helper functions for multiplication and division *)
let scalar_mult_list (l1 : int list) (c : int) =
  List.map  (function x -> c*x) l1;;

let scalar_div_list (l1 : int list) (d : int) =
  let rec simple_div l1 l2 crry dvsr = match l1 with
      [] -> l2
    |x :: xs -> simple_div xs ((crry*10+x)/d :: l2) ((crry*10+x) mod d) dvsr
  in List.rev (simple_div l1 [] 0 d);;

let left_shift (l1 : int list) (shift : int) =
  let rec left_rec_shift (l1 : int list) (z : int list) (shift : int) = match shift with
      0 -> l1 @ z
    | _ -> left_rec_shift l1 (0::z) (shift+(-1))
  in left_rec_shift l1 [] shift;;

let right_shift  (l1 : int list) (shift : int) =
  let rec right_rec_shift rev_l1 shift = match shift with
      0 -> rev_l1;
    | _ -> match rev_l1 with
        [] -> []
      | x :: xs -> right_rec_shift xs (shift + (-1))
  in List.rev (right_rec_shift (List.rev l1) shift);;

(* Multiplication *)
let mult (a: bigint) (b: bigint) =
  let rec mult_const_add l1 rev_l2 ml lev = match rev_l2 with
      [] -> ml
    | x :: xs -> mult_const_add (left_shift l1 1) (xs)  (propogate_carry((simple_add ml (scalar_mult_list l1 x)))) (lev+1)
  in bigintmake (sp (fst a) (fst b)) (mult_const_add (snd a) (List.rev (snd b)) [] 0);;

(* Quotient *)
let div (a: bigint) (b : bigint) =
  let rec div_const_add l1 rev_l2 dl lev = match rev_l2 with
      [] -> dl
    |x :: xs -> div_const_add (right_shift rev_l2 1) (xs) (propogate_carry((simple_add dl (scalar_div_list l1 x)))) (lev+1)
  in bigintmake (sp (fst a) (fst b)) (div_const_add (snd a) (List.rev (snd b)) [] 0);;

(* Remainder *)

(* Absolute Value *)
let abs (a: bigint) : bigint = if (fst a) = NonNeg then a
  else bigintmake (NonNeg) (snd a);;


(* ---------------------------------------------- *)
(* COMPARISON OPERATIONS *)

(* Equal *)
let eq (a : bigint) (b : bigint) =
  let rec match_list l1 l2 b = match (l1, l2, b) with
    (_, _, false) -> false
    |([], [], true) -> true
    |([], _, true) -> false
    |(_, [], true) -> false
    | (x :: xs, y :: ys, true) -> match_list xs ys (x=y)
  in ((fst a) == (fst b)) && (match_list (snd a) (snd b) true);;

let ( = ) (a : bigint) (b : bigint) = eq a b;;

(* Greater or Equal *)
let geq (a : bigint) (b : bigint) =
  let rec larger_list l1 l2 c = match (l1, l2) with
      (x:: xs, y::ys) -> larger_list xs ys (c&&(x>=y))
    | ([], y::ys) -> false
    | (x::xs, []) -> true
    | ([], []) -> c
  in match (fst a, fst b) with
     (NonNeg, Neg) -> true
    |(Neg, NonNeg) -> false
    |(NonNeg, NonNeg)-> larger_list (snd a) (snd b) true
    |(Neg, Neg) -> larger_list (snd b) (snd a) true;;

(* Greater Than *)
let gt (a : bigint) (b : bigint) =
  (geq a b) && not (eq a b);;

let ( > ) (a : bigint) (b : bigint) = gt a b;;

(* Less Than *)
let lt (a : bigint) (b : bigint) =
  (gt b a);;

let ( < ) (a : bigint) (b : bigint) = lt a b;;

(* Lesser or Equal *)
let leq (a : bigint) (b : bigint) =
  (geq b a);;


(* ---------------------------------------------- *)
(* CONVERTING TO A STRING TO PRINT *)
let print_num (a : bigint) =
  let rec print_num_list s l = match l with
    [] -> (match s with
        "" -> "0"
        | _ -> s)
    | hd :: tl -> print_num_list (s^string_of_int(hd)) tl
  in if (fst a == Neg) then print_num_list "-" (snd a)
  else print_num_list "" (snd a);;

(* ---------------------------------------------- *)
(* CONVERTING INT TO BIGINT *)

(* Get the int list component of big int *)
let dig_list (a : int) =
  let rec make_int_list (a:int) (l:int list) = if a == 0 then l else make_int_list (a/10) ((a mod 10) :: l)
  in make_int_list a [];;

(* Converting using dig_list *)
let mk_big (a : int) :bigint = if a >= 0 then (NonNeg, dig_list a) else (Neg, dig_list (-a)) ;;
