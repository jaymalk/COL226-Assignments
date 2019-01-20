(* Assignment 0 *)


(* ---------------------------------------------- *)
(* Declaring the types *)
type sign = Neg | NonNeg ;;
type bigint = sign * int list;;
(* Constructor of bigint *)
let bigintmake (s : sign) (l : int list) : bigint = (s, l);;


(* ---------------------------------------------- *)
(* ARITHMETIC OPERATIONS *)

(* Addition *)
(* let add (a: bigint) (b: bigint) :bigint =
    let rec add_dig a b c = *)

(* Multiplication *)

(* Subtraction *)

(* Quotient *)

(* Remainder *)

(* Unary Negation *)
let minus (a: bigint) = if (fst a) = NonNeg then bigintmake (Neg) (snd a)
                        else bigintmake (NonNeg) (snd a);;

(* Absolute Value *)
let abs (a: bigint) = if (fst a) = NonNeg then a
                      else bigintmake (NonNeg) (snd a);;


(* ---------------------------------------------- *)
(* COMPARISON OPERATIONS *)

(* Equal *)
let eq (a : bigint) (b : bigint) = if (snd a = []) then (snd b=[]) else
    let rec equal_list l1 l2 b = match b with
        false -> false
        | true -> (
            match l1 with
                [] -> (l2 = [])
                | _ -> equal_list (List.tl l1) (List.tl l2) (b && ((List.hd l1) = (List.hd l2)) )
            )
    in ((fst a) = (fst b)) && (equal_list (snd a) (snd b) true);;

let ( = ) (a : bigint) (b : bigint) = eq a b;;

(* Greater Than *)

(* Less Than *)

(* Greater or Equal *)

(* Less or Equals *)


(* ---------------------------------------------- *)
(* CONVERTING TO A STRING TO PRINT *)
let print_num (a : bigint) =
    let rec print_num_list s l = match l with
        [] -> s
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
