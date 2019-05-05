exception Not_Implemented of string
exception Error of string
exception Variable_Count_Mismatch
exception Bad_State of string

type types = Tint

type variable = Var of string * types

type procedure = Procedure of string * variable list * variable list * procedure list;;

type gamma = (string * int) list;;

type call = Call of string * intype list | Access of string | Stack_Trace | Set of string * intype
and intype = N of int | V of string;;

type opcode =
   (* Stack opcodes *)
  STATIC of int | LOCAL of opcode list | PASSED of opcode list | START of string * int (*| RET of int*)
   (* String opcode *)
  | MAP of string * int
  and
frame = opcode list;;

(* Hardcoded frames linked to their names *)
type frame_sets = (string * frame) list;;

type stack = opcode list;;