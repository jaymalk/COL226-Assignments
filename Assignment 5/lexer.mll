(*
    LEXER
    - For lexing the linear string from stdin
    - Uses tokens defined and returns them back to the parser
*)

(* Main file declarations *)
{
  open Parser
  open Bigint
  exception Not_implemented
  exception Compiler_Start
  exception Bad_Char of char
}

let id = ['A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*

let sp = [' ' '\t' '\n']+

(* INTEGERS *)
let integer = ['0'-'9']+


(* Rule for parsing the tokens *)
rule read = parse
    sp                  { read lexbuf  (* Ignoring white space. *)}

|   '$'                 { raise Compiler_Start (* Prompting the work of compiler. *)}

|   '('                 { LP        (* Left Parathesis *)}
|   ')'                 { RP        (* Right Parathesis *)}

|   "cmp"               { CMP       (* Comparing with zero *)}

|   "while"             { WHILE (* While loops *)}
|   '{'                 { LC        (* Left Parathesis *)}
|   '}'                 { RC        (* Right Parathesis *)}

|   '['                 { LS        (* Left Parathesis *)}
|   ']'                 { RS        (* Right Parathesis *)}

|   ','                 { COMMA     (* Seperator for N-Tuples *)}
|   "proj"              { PROJ      (* Projector Syntax for N-Tuples *)}
|   '/'                 { SLASH }

|   "def"               { DEF       (* Definitional Keyword *)}
|   '#'                 { HASH }
|   ':'                 { SERIES }
|   '|'                 { PARALLEL }
|   "func"              { FUNC }
|   "rec"               { REC }
|   "->"                { ARROW }
|   '\\'                { BACKSLASH }
|   '.'                 { DOT }

|   "let"               { LET }
|   "in"                { IN    (* Local definitions in expressions *)}
|   "end"               { END }
|   "local"             { LOCAL }

|   ';'                 { EOC (* Command Delimiter *)}
|   '?'                 { WATCH (* For viewing table contents *)}
|   "exit"              { EXIT (* Exiting the interpreter *)}

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
|   ">"                 { GT        (* Comparison Keywords *)}
|   "<"                 { LT }

|   'T'                 { BOOL(true) }
|   'F'                 { BOOL(false)  (* Boolean constants *)}

|   id as i             { ID (i)      (* Variable IDs *)}

|   ['0'-'9']+ as n     { INT(int_of_string n) (*INTEGER TYPE*) }
(* | integer as i          {(INT(Bigint.bigint_of_string i)) (* INTEGER TYPE *) } *)

|   eof                 { EOF          (* End of file marker *)}

|   _ as s              { raise (Bad_Char(s)) }
