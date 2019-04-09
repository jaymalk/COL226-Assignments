type token =
  | LP
  | RP
  | COMMA
  | PROJ
  | IF
  | THEN
  | ELSE
  | FI
  | TILDA
  | ABS
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | REM
  | NOT
  | CONJ
  | DISJ
  | EQ
  | GT
  | LT
  | BACKSLASH
  | DOT
  | COLON
  | ARROW
  | BOOL of (bool)
  | INT of (int)
  | ID of (string)
  | TT
  | TB
  | TP
  | TF
  | DEF
  | LET
  | IN
  | END
  | LOCAL
  | SEMICOLON
  | PARALLEL
  | DELIMITER
  | EOF

val def_parser :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> A1.definition
val exp_parser :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> A1.exptree
val type_parser :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> A1.exptype
