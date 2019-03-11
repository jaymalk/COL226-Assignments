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
  | GTA
  | LTA
  | GEQ
  | LEQ
  | BOOL of (bool)
  | INT of (int)
  | ID of (string)
  | DEF
  | DELIMITER
  | EOF

open Parsing;;
let _ = parse_error;;
# 9 "a3.mly"
    open A1
    exception Bad_State
# 38 "a3.ml"
let yytransl_const = [|
  257 (* LP *);
  258 (* RP *);
  259 (* COMMA *);
  260 (* PROJ *);
  261 (* IF *);
  262 (* THEN *);
  263 (* ELSE *);
  264 (* FI *);
  265 (* TILDA *);
  266 (* ABS *);
  267 (* PLUS *);
  268 (* MINUS *);
  269 (* TIMES *);
  270 (* DIV *);
  271 (* REM *);
  272 (* NOT *);
  273 (* CONJ *);
  274 (* DISJ *);
  275 (* EQ *);
  276 (* GTA *);
  277 (* LTA *);
  278 (* GEQ *);
  279 (* LEQ *);
  283 (* DEF *);
  284 (* DELIMITER *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  280 (* BOOL *);
  281 (* INT *);
  282 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\001\000\002\000\002\000\002\000\003\000\
\003\000\008\000\008\000\004\000\007\000\005\000\005\000\005\000\
\009\000\009\000\010\000\010\000\011\000\011\000\012\000\012\000\
\013\000\013\000\014\000\014\000\014\000\006\000\006\000\016\000\
\016\000\016\000\016\000\016\000\015\000\015\000\017\000\017\000\
\018\000\018\000\019\000\019\000\000\000"

let yylen = "\002\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\002\000\
\003\000\001\000\003\000\007\000\007\000\001\000\002\000\002\000\
\003\000\001\000\003\000\001\000\003\000\001\000\003\000\001\000\
\003\000\001\000\001\000\001\000\003\000\001\000\001\000\003\000\
\003\000\003\000\003\000\003\000\003\000\001\000\003\000\001\000\
\002\000\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\043\000\027\000\028\000\001\000\045\000\002\000\003\000\004\000\
\000\000\006\000\007\000\014\000\000\000\000\000\000\000\000\000\
\000\000\030\000\031\000\000\000\000\000\042\000\008\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\016\000\015\000\000\000\041\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\029\000\044\000\009\000\000\000\000\000\000\000\000\000\
\000\000\032\000\033\000\034\000\035\000\036\000\017\000\019\000\
\021\000\023\000\025\000\037\000\039\000\011\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\012\000\013\000"

let yydgoto = "\002\000\
\032\000\014\000\015\000\016\000\017\000\018\000\019\000\035\000\
\020\000\021\000\022\000\023\000\024\000\025\000\026\000\027\000\
\028\000\029\000\030\000"

let yysindex = "\009\000\
\014\000\000\000\001\000\012\255\048\255\002\255\002\255\028\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\065\255\000\000\000\000\000\000\011\255\006\255\008\255\037\255\
\009\255\000\000\000\000\035\255\038\255\000\000\000\000\022\255\
\057\255\052\255\054\255\040\255\048\255\065\255\056\255\010\255\
\000\000\000\000\048\255\000\000\010\255\010\255\010\255\010\255\
\010\255\002\255\002\255\002\255\002\255\002\255\014\255\014\255\
\014\000\000\000\000\000\000\000\063\255\057\255\052\255\007\255\
\058\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\042\255\062\255\
\068\255\007\255\070\255\067\255\001\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\063\000\000\000\000\000\000\000\112\000\090\000\083\000\061\000\
\039\000\000\000\000\000\006\000\026\000\000\000\000\000\079\255\
\086\255\088\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\081\000\242\255\010\000\000\000\253\255\004\000\000\000\035\000\
\254\255\043\000\044\000\042\000\045\000\000\000\046\000\000\000\
\041\000\000\000\092\000"

let yytablesize = 391
let yytable = "\033\000\
\012\000\038\000\040\000\041\000\042\000\038\000\034\000\037\000\
\039\000\001\000\040\000\005\000\036\000\012\000\043\000\006\000\
\007\000\051\000\006\000\007\000\052\000\050\000\008\000\054\000\
\057\000\040\000\010\000\011\000\043\000\008\000\009\000\010\000\
\011\000\062\000\010\000\011\000\065\000\009\000\026\000\038\000\
\063\000\066\000\067\000\068\000\069\000\070\000\063\000\071\000\
\037\000\080\000\053\000\009\000\055\000\059\000\056\000\060\000\
\006\000\007\000\058\000\058\000\024\000\064\000\005\000\008\000\
\061\000\079\000\081\000\084\000\082\000\083\000\085\000\009\000\
\010\000\011\000\087\000\045\000\046\000\047\000\048\000\049\000\
\010\000\013\000\022\000\045\000\046\000\047\000\048\000\049\000\
\005\000\020\000\006\000\078\000\086\000\072\000\074\000\073\000\
\077\000\000\000\075\000\044\000\076\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\018\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\003\000\031\000\000\000\004\000\005\000\000\000\038\000\
\038\000\006\000\007\000\038\000\038\000\038\000\003\000\000\000\
\008\000\004\000\005\000\000\000\000\000\000\000\006\000\007\000\
\009\000\010\000\011\000\040\000\040\000\008\000\000\000\040\000\
\040\000\040\000\000\000\000\000\000\000\009\000\010\000\011\000\
\026\000\026\000\000\000\040\000\026\000\026\000\026\000\000\000\
\000\000\026\000\026\000\026\000\026\000\000\000\000\000\000\000\
\000\000\026\000\026\000\026\000\026\000\026\000\024\000\024\000\
\005\000\005\000\024\000\024\000\024\000\005\000\005\000\024\000\
\024\000\024\000\000\000\000\000\000\000\000\000\000\000\024\000\
\024\000\024\000\024\000\024\000\022\000\022\000\000\000\000\000\
\022\000\022\000\022\000\020\000\020\000\022\000\022\000\020\000\
\020\000\020\000\000\000\000\000\020\000\022\000\022\000\022\000\
\022\000\022\000\000\000\000\000\020\000\020\000\020\000\020\000\
\020\000\018\000\018\000\000\000\000\000\018\000\018\000\018\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\018\000\018\000\018\000\018\000\018\000"

let yycheck = "\003\000\
\000\000\005\000\001\001\006\000\007\000\000\000\003\000\001\001\
\005\000\001\000\001\001\005\001\001\001\000\000\001\001\009\001\
\010\001\012\001\009\001\010\001\013\001\011\001\016\001\015\001\
\003\001\000\000\025\001\026\001\001\001\016\001\024\001\025\001\
\026\001\037\000\025\001\026\001\040\000\024\001\000\000\043\000\
\037\000\045\000\046\000\047\000\048\000\049\000\043\000\050\000\
\001\001\064\000\014\001\024\001\018\001\002\001\017\001\002\001\
\009\001\010\001\002\001\002\001\000\000\006\001\000\000\016\001\
\025\001\003\001\025\001\082\000\007\001\002\001\001\001\024\001\
\025\001\026\001\008\001\019\001\020\001\021\001\022\001\023\001\
\002\001\001\000\000\000\019\001\020\001\021\001\022\001\023\001\
\003\001\000\000\003\001\057\000\083\000\051\000\053\000\052\000\
\056\000\255\255\054\000\008\000\055\000\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\000\000\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\001\001\002\001\255\255\004\001\005\001\255\255\002\001\
\003\001\009\001\010\001\006\001\007\001\008\001\001\001\255\255\
\016\001\004\001\005\001\255\255\255\255\255\255\009\001\010\001\
\024\001\025\001\026\001\002\001\003\001\016\001\255\255\006\001\
\007\001\008\001\255\255\255\255\255\255\024\001\025\001\026\001\
\002\001\003\001\255\255\018\001\006\001\007\001\008\001\255\255\
\255\255\011\001\012\001\013\001\014\001\255\255\255\255\255\255\
\255\255\019\001\020\001\021\001\022\001\023\001\002\001\003\001\
\002\001\003\001\006\001\007\001\008\001\007\001\008\001\011\001\
\012\001\013\001\255\255\255\255\255\255\255\255\255\255\019\001\
\020\001\021\001\022\001\023\001\002\001\003\001\255\255\255\255\
\006\001\007\001\008\001\002\001\003\001\011\001\012\001\006\001\
\007\001\008\001\255\255\255\255\011\001\019\001\020\001\021\001\
\022\001\023\001\255\255\255\255\019\001\020\001\021\001\022\001\
\023\001\002\001\003\001\255\255\255\255\006\001\007\001\008\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\019\001\020\001\021\001\022\001\023\001"

let yynames_const = "\
  LP\000\
  RP\000\
  COMMA\000\
  PROJ\000\
  IF\000\
  THEN\000\
  ELSE\000\
  FI\000\
  TILDA\000\
  ABS\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIV\000\
  REM\000\
  NOT\000\
  CONJ\000\
  DISJ\000\
  EQ\000\
  GTA\000\
  LTA\000\
  GEQ\000\
  LEQ\000\
  DEF\000\
  DELIMITER\000\
  EOF\000\
  "

let yynames_block = "\
  BOOL\000\
  INT\000\
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    Obj.repr(
# 48 "a3.mly"
                                  (Done)
# 283 "a3.ml"
               : A1.exptree))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'premain) in
    Obj.repr(
# 49 "a3.mly"
                                  ( _1 )
# 290 "a3.ml"
               : A1.exptree))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'tuple) in
    Obj.repr(
# 50 "a3.mly"
                                    ( _1 )
# 297 "a3.ml"
               : A1.exptree))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'projections) in
    Obj.repr(
# 51 "a3.mly"
                                    ( _1 )
# 304 "a3.ml"
               : A1.exptree))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arithmetic_expression) in
    Obj.repr(
# 54 "a3.mly"
                                  ( _1 )
# 311 "a3.ml"
               : 'premain))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'boolean_expression) in
    Obj.repr(
# 55 "a3.mly"
                                  ( _1 )
# 318 "a3.ml"
               : 'premain))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'conditional) in
    Obj.repr(
# 56 "a3.mly"
                                  ( _1 )
# 325 "a3.ml"
               : 'premain))
; (fun __caml_parser_env ->
    Obj.repr(
# 60 "a3.mly"
                                  (Tuple(0, []))
# 331 "a3.ml"
               : 'tuple))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'tuple_list) in
    Obj.repr(
# 61 "a3.mly"
                                  ( _2 )
# 338 "a3.ml"
               : 'tuple))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : A1.exptree) in
    Obj.repr(
# 64 "a3.mly"
                 (Tuple(1, [_1]))
# 345 "a3.ml"
               : 'tuple_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : A1.exptree) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'tuple_list) in
    Obj.repr(
# 65 "a3.mly"
                            (match _3 with Tuple(x, el) -> Tuple(x+1, _1::el) | _ -> raise Bad_State)
# 353 "a3.ml"
               : 'tuple_list))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : int) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'tuple) in
    Obj.repr(
# 69 "a3.mly"
                                   (Project((_3, _5), _7))
# 362 "a3.ml"
               : 'projections))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : 'boolean_expression) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'premain) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'premain) in
    Obj.repr(
# 73 "a3.mly"
                                                          (IfThenElse(_2, _4, _6))
# 371 "a3.ml"
               : 'conditional))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'add_expression) in
    Obj.repr(
# 77 "a3.mly"
                                  ( _1 )
# 378 "a3.ml"
               : 'arithmetic_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'add_expression) in
    Obj.repr(
# 78 "a3.mly"
                                  (Abs(_2))
# 385 "a3.ml"
               : 'arithmetic_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'add_expression) in
    Obj.repr(
# 79 "a3.mly"
                                  (Negative(_2))
# 392 "a3.ml"
               : 'arithmetic_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'sub_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'add_expression) in
    Obj.repr(
# 82 "a3.mly"
                                              (Add(_1, _3))
# 400 "a3.ml"
               : 'add_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'sub_expression) in
    Obj.repr(
# 83 "a3.mly"
                                              ( _1 )
# 407 "a3.ml"
               : 'add_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'mult_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'sub_expression) in
    Obj.repr(
# 86 "a3.mly"
                                              (Sub(_1, _3))
# 415 "a3.ml"
               : 'sub_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'mult_expression) in
    Obj.repr(
# 87 "a3.mly"
                                              ( _1 )
# 422 "a3.ml"
               : 'sub_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'div_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'mult_expression) in
    Obj.repr(
# 90 "a3.mly"
                                              (Mult(_1, _3))
# 430 "a3.ml"
               : 'mult_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'div_expression) in
    Obj.repr(
# 91 "a3.mly"
                                              ( _1 )
# 437 "a3.ml"
               : 'mult_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'rem_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'div_expression) in
    Obj.repr(
# 94 "a3.mly"
                                              (Div(_1, _3))
# 445 "a3.ml"
               : 'div_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'rem_expression) in
    Obj.repr(
# 95 "a3.mly"
                                              ( _1 )
# 452 "a3.ml"
               : 'div_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'basic_int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'rem_expression) in
    Obj.repr(
# 98 "a3.mly"
                                              (Rem(_1, _3))
# 460 "a3.ml"
               : 'rem_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'basic_int) in
    Obj.repr(
# 99 "a3.mly"
                                              ( _1 )
# 467 "a3.ml"
               : 'rem_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 102 "a3.mly"
                                              (N(_1))
# 474 "a3.ml"
               : 'basic_int))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 103 "a3.mly"
                                              (Var(_1))
# 481 "a3.ml"
               : 'basic_int))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'arithmetic_expression) in
    Obj.repr(
# 104 "a3.mly"
                                              (InParen(_2))
# 488 "a3.ml"
               : 'basic_int))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'or_expression) in
    Obj.repr(
# 108 "a3.mly"
                                              ( _1 )
# 495 "a3.ml"
               : 'boolean_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'comparison) in
    Obj.repr(
# 109 "a3.mly"
                                              ( _1 )
# 502 "a3.ml"
               : 'boolean_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arithmetic_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arithmetic_expression) in
    Obj.repr(
# 113 "a3.mly"
                                                        (Equals(_1, _3))
# 510 "a3.ml"
               : 'comparison))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arithmetic_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arithmetic_expression) in
    Obj.repr(
# 114 "a3.mly"
                                                        (GreaterT(_1, _3))
# 518 "a3.ml"
               : 'comparison))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arithmetic_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arithmetic_expression) in
    Obj.repr(
# 115 "a3.mly"
                                                        (LessT(_1, _3))
# 526 "a3.ml"
               : 'comparison))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arithmetic_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arithmetic_expression) in
    Obj.repr(
# 116 "a3.mly"
                                                        (GreaterTE(_1, _3))
# 534 "a3.ml"
               : 'comparison))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arithmetic_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arithmetic_expression) in
    Obj.repr(
# 117 "a3.mly"
                                                        (LessTE(_1, _3))
# 542 "a3.ml"
               : 'comparison))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'and_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'or_expression) in
    Obj.repr(
# 121 "a3.mly"
                                              (Disjunction(_1, _3))
# 550 "a3.ml"
               : 'or_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'and_expression) in
    Obj.repr(
# 122 "a3.mly"
                                              ( _1 )
# 557 "a3.ml"
               : 'or_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'not_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'and_expression) in
    Obj.repr(
# 125 "a3.mly"
                                              (Conjunction(_1, _3))
# 565 "a3.ml"
               : 'and_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'not_expression) in
    Obj.repr(
# 126 "a3.mly"
                                              ( _1 )
# 572 "a3.ml"
               : 'and_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'basic_bool) in
    Obj.repr(
# 129 "a3.mly"
                                              (Not(_2))
# 579 "a3.ml"
               : 'not_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'basic_bool) in
    Obj.repr(
# 130 "a3.mly"
                                              ( _1 )
# 586 "a3.ml"
               : 'not_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 133 "a3.mly"
                                              ( B(_1) )
# 593 "a3.ml"
               : 'basic_bool))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'boolean_expression) in
    Obj.repr(
# 134 "a3.mly"
                                              (InParen(_2))
# 600 "a3.ml"
               : 'basic_bool))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : A1.exptree)
