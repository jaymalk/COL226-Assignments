(* Assignment 0 *)


open Signature_a0
module A0 : BigInt = struct
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
let minus_list (l : int list) = List.map ( function x -> (-x) ) l;; (* Negate all ints in a list *)



(* Exceptions *)



(* Helper function for comparing int lists *)
let larger_or_equal_list l1 l2 =
  let len1 = List.length l1 and
      len2 = List.length l2
  in match (len1 + (-len2)) with
    0 ->
    let rec compare_values l1 l2 = match (l1, l2) with
        ([], []) -> true
      |(x::xs, y::ys) -> if (x > y) then true else if (x < y) then false else (compare_values xs ys)
      | _ -> false
    in compare_values l1 l2
  | _ -> len1 > len2


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
  let rec add_till_end l1 l2 la = match (l1, l2) with
      ([], []) -> remove_zeros(propogate_carry(la))
    |(x::xs, []) -> add_till_end xs [] (x::la)
    |([], y::ys) -> add_till_end [] ys (y::la)
    |(x::xs, y::ys) -> add_till_end xs ys ((x+y)::la)
  in add_till_end (List.rev l1) (List.rev l2) [];;

let simple_subtract (l1 : int list) (l2 : int list) : bigint = match (larger_or_equal_list l1 l2) with
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
  let rec left_rec_shift (l1 : int list) (z : int list) (shift : int) = match shift>0 with
      false -> l1 @ z
    | _ -> left_rec_shift l1 (0::z) (shift+(-1))
  in left_rec_shift l1 [] shift;;

let right_shift  (l1 : int list) (shift : int) =
  let rec right_rec_shift rev_l1 shift = match shift > 0 with
      false -> rev_l1;
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
let div_by_int (a: bigint) (b : int) =
  let sgn a b  = match (b >=0) with
      true -> sp (fst a) NonNeg
    |false -> sp (fst a) Neg
  in bigintmake (sgn a b) (remove_zeros(propogate_carry(scalar_div_list (snd a) b)));;

let fast_div_rem (l1 : int list) (l2 : int list) =
  let rec div_by_shift l1 l2 ld ladd =
    match (ladd, larger_or_equal_list l1 l2) with
      ([], _) -> (ld, l1)
    | (_, true) -> div_by_shift (simple_add (l1) (minus_list l2)) l2 (simple_add ld ladd) ladd
    | (_, false) -> div_by_shift (l1) (right_shift l2 1) (ld) (right_shift ladd 1)
  in
  let len1 = List.length (l1) and
    len2 = List.length (l2) in
  div_by_shift (l1) (left_shift (l2) (len1 - len2)) [] (left_shift [1] (len1 - len2)) ;;

let div (a : bigint) (b : bigint) = match fst a = fst b with
    true -> bigintmake (NonNeg) (fst (fast_div_rem (snd a) (snd b)))
  | false -> bigintmake (Neg)  (fst (fast_div_rem (snd a) (snd b)))
;;

(* Remainder *)
let rem (a : bigint) (b : bigint) = match fst a = NonNeg with
    true -> bigintmake (NonNeg) (snd (fast_div_rem (snd a) (snd b)))
  | false -> bigintmake (Neg) (snd (fast_div_rem (snd a) (snd b)))
;;

(* Absolute Value *)
let abs (a: bigint) : bigint = if (fst a) = NonNeg then a
  else bigintmake (NonNeg) (snd a);;


(* ---------------------------------------------- *)
(* COMPARISON OPERATIONS *)

(* Equal *)
let eq (a : bigint) (b : bigint) =
  if (snd a = []) then (snd b = []) else
  let rec match_list l1 l2 b = match (l1, l2, b) with
      (_, _, false) -> false
    |([], [], true) -> true
    |([], _, true) -> false
    |(_, [], true) -> false
    | (x :: xs, y :: ys, true) -> match_list xs ys (x=y)
  in ((fst a) == (fst b)) && (match_list (snd a) (snd b) true);;

(* Greater or Equal *)
let geq (a : bigint) (b : bigint) =
  match (fst a, fst b) with
    (NonNeg, Neg) -> true
  |(Neg, NonNeg) -> false
  |(NonNeg, NonNeg)-> larger_or_equal_list (snd a) (snd b)
  |(Neg, Neg) -> larger_or_equal_list (snd b) (snd a);;

(* Greater Than *)
let gt (a : bigint) (b : bigint) =
  (geq a b) && not (eq a b);;

(* Less Than *)
let lt (a : bigint) (b : bigint) =
  (gt b a);;

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

end
