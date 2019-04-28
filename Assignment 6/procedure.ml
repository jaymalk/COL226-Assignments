
type types = Tint | Tbool

type variable = Var of string * types

type procedure = Procedure of string * variable list * variable list * procedure list;;

type program = Main of variable list * procedure list;;
