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
  | BOOL of (bool)
  | INT of (int)
  | ID of (string)
  | DEF
  | DELIMITER
  | EOF

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> A1.exptree
