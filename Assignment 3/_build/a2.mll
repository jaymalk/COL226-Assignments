(*
    LEXER
    - For lexing the linear string from stdin
    - Uses tokens defined and returns them back to the parser
*)

(* Main file declarations *)
{
  open A3
  exception Not_implemented
  exception Bad_State
}

let id = ['A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*

let sp = [' ' '\t' '\n']+

let digit = ['0'-'9']
let integer = ('-'|'+')?(['1'-'9']digit* | '0')


(* Rule for parsing the tokens *)
rule read = parse
    sp                  { read lexbuf  (* Ignoring white space. *)}

|   '('                 { LP        (* Left Parathesis *)}
|   ')'                 { RP        (* Right Parathesis *)}

|   ','                 { COMMA     (* Seperator for N-Tuples *)}
|   "proj"              { PROJ      (* Projector Syntax for N-Tuples *)}

|   "def"               { DEF       (* Definitional Keyword *)}

|   '$'                 { DELIMITER (* Command Delimiter *)}

|   '~'                 { TILDA     (* Unary Minus *)}
|   "abs"               { ABS       (* Absolute Value *)}

|   '+'                 { PLUS }
|   '-'                 { MINUS }
|   '*'                 { TIMES     (* Binary Arithmetic Operation Keywords *)}
|   "div"               { DIV }
|   "mod"               { REM }

|   "not"               { NOT       (* Logical Negation *)}

|   "\\/"               { DISJ      (* Binary Boolean Operation Keywords *)}
|   "/\\"               { CONJ }

|   "if"                { IF }
|   "then"              { THEN      (* Conditional Statement Keywords *)}
|   "else"              { ELSE }
|   "fi"                { FI }

|   "="                 { EQ }
|   ">"                 { GTA }
|   "<"                 { LTA       (* Comparison Keywords *)}
|   ">="                { GEQ }
|   "<="                { LEQ }

|   'T'                 { BOOL(true) }
|   'F'                 { BOOL(false)  (* Boolean constants *)}

|   id as i             { ID (i)      (* Variable IDs *)}

|   integer as i        {match i.[0] with '+' -> (INT(int_of_string (String.sub i 1 (String.length i - 1))))| _ -> (INT(int_of_string i)) (* INTEGER TYPE *)}

|   eof                 { EOF          (* End of file marker *)}

|   _                   { raise Bad_State}
