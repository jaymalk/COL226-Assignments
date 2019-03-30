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

open Parsing;;
let _ = parse_error;;
# 9 "a3.mly"
    open A1
    exception Bad_State
# 36 "a3.ml"
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
  276 (* GT *);
  277 (* LT *);
  281 (* DEF *);
  282 (* DELIMITER *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  278 (* BOOL *);
  279 (* INT *);
  280 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\006\000\004\000\004\000\007\000\
\007\000\005\000\003\000\009\000\009\000\009\000\009\000\009\000\
\008\000\008\000\011\000\011\000\012\000\012\000\012\000\010\000\
\013\000\013\000\014\000\014\000\015\000\015\000\016\000\016\000\
\017\000\017\000\018\000\018\000\018\000\019\000\019\000\019\000\
\019\000\019\000\000\000"

let yylen = "\002\000\
\002\000\001\000\001\000\001\000\007\000\002\000\003\000\003\000\
\003\000\007\000\001\000\003\000\003\000\003\000\004\000\004\000\
\003\000\001\000\003\000\001\000\002\000\001\000\001\000\001\000\
\003\000\001\000\003\000\001\000\003\000\001\000\003\000\001\000\
\003\000\001\000\001\000\002\000\002\000\001\000\001\000\001\000\
\003\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\039\000\038\000\040\000\043\000\000\000\002\000\003\000\004\000\
\042\000\011\000\023\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\034\000\035\000\006\000\000\000\000\000\
\000\000\000\000\000\000\037\000\036\000\021\000\001\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\041\000\000\000\007\000\000\000\000\000\000\000\012\000\
\000\000\013\000\000\000\014\000\017\000\019\000\000\000\000\000\
\000\000\000\000\033\000\000\000\009\000\000\000\000\000\015\000\
\016\000\000\000\000\000\000\000\000\000\010\000\005\000"

let yydgoto = "\002\000\
\012\000\013\000\014\000\015\000\016\000\017\000\032\000\018\000\
\019\000\020\000\021\000\022\000\023\000\024\000\025\000\026\000\
\027\000\028\000\029\000"

let yysindex = "\005\000\
\054\255\000\000\006\255\008\255\054\255\121\255\121\255\121\255\
\000\000\000\000\000\000\000\000\019\000\000\000\000\000\000\000\
\000\000\000\000\000\000\249\254\002\255\007\255\012\255\013\255\
\014\255\017\255\011\255\000\000\000\000\000\000\015\255\031\255\
\016\255\028\255\054\255\000\000\000\000\000\000\000\000\121\255\
\070\255\081\255\101\255\101\255\121\255\121\255\121\255\121\255\
\121\255\000\000\054\255\000\000\035\255\054\255\038\255\000\000\
\121\255\000\000\121\255\000\000\000\000\000\000\013\255\014\255\
\017\255\011\255\000\000\039\255\000\000\020\255\037\255\000\000\
\000\000\043\255\054\255\054\255\041\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\088\000\128\000\108\000\148\000\121\000\
\081\000\041\000\001\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\141\000\101\000\
\061\000\021\000\000\000\044\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\253\255\000\000\000\000\000\000\000\000\255\255\010\000\
\000\000\251\255\012\000\000\000\000\000\015\000\011\000\018\000\
\014\000\254\255\000\000"

let yytablesize = 425
let yytable = "\031\000\
\032\000\034\000\038\000\036\000\037\000\001\000\003\000\030\000\
\033\000\004\000\005\000\040\000\041\000\042\000\006\000\007\000\
\050\000\051\000\039\000\043\000\031\000\008\000\045\000\044\000\
\046\000\049\000\047\000\009\000\010\000\011\000\048\000\055\000\
\052\000\054\000\056\000\058\000\060\000\070\000\053\000\050\000\
\030\000\051\000\074\000\075\000\076\000\008\000\067\000\068\000\
\079\000\069\000\071\000\072\000\061\000\073\000\003\000\062\000\
\064\000\004\000\005\000\063\000\029\000\066\000\006\000\007\000\
\065\000\000\000\000\000\000\000\000\000\008\000\035\000\077\000\
\078\000\000\000\005\000\009\000\010\000\011\000\006\000\007\000\
\028\000\035\000\000\000\000\000\000\000\005\000\000\000\022\000\
\057\000\006\000\007\000\009\000\010\000\011\000\000\000\000\000\
\000\000\000\000\000\000\059\000\027\000\035\000\009\000\010\000\
\011\000\005\000\000\000\020\000\000\000\006\000\007\000\000\000\
\000\000\000\000\000\000\000\000\008\000\000\000\000\000\000\000\
\026\000\035\000\009\000\010\000\011\000\005\000\000\000\018\000\
\000\000\006\000\007\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\025\000\000\000\009\000\010\000\
\011\000\000\000\000\000\024\000\000\000\000\000\000\000\000\000\
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
\000\000\000\000\032\000\032\000\000\000\000\000\032\000\032\000\
\032\000\000\000\000\000\032\000\032\000\032\000\032\000\000\000\
\000\000\032\000\032\000\032\000\032\000\032\000\031\000\031\000\
\000\000\000\000\031\000\031\000\031\000\000\000\000\000\031\000\
\031\000\031\000\031\000\000\000\000\000\031\000\031\000\031\000\
\031\000\031\000\030\000\030\000\000\000\000\000\030\000\030\000\
\030\000\000\000\000\000\030\000\030\000\030\000\000\000\000\000\
\000\000\030\000\030\000\030\000\030\000\030\000\029\000\029\000\
\000\000\000\000\029\000\029\000\029\000\000\000\000\000\029\000\
\029\000\029\000\000\000\000\000\000\000\029\000\029\000\029\000\
\029\000\029\000\028\000\028\000\000\000\000\000\028\000\028\000\
\028\000\022\000\022\000\028\000\028\000\022\000\022\000\022\000\
\000\000\028\000\028\000\028\000\028\000\028\000\027\000\027\000\
\022\000\022\000\027\000\027\000\027\000\020\000\020\000\027\000\
\027\000\020\000\020\000\020\000\000\000\027\000\027\000\027\000\
\027\000\027\000\026\000\026\000\000\000\020\000\026\000\026\000\
\026\000\018\000\018\000\026\000\000\000\018\000\018\000\018\000\
\000\000\026\000\026\000\026\000\026\000\026\000\025\000\025\000\
\000\000\000\000\025\000\025\000\025\000\024\000\024\000\025\000\
\000\000\024\000\024\000\024\000\000\000\025\000\025\000\025\000\
\025\000\025\000\000\000\000\000\024\000\024\000\024\000\024\000\
\024\000"

let yycheck = "\003\000\
\000\000\005\000\008\000\006\000\007\000\001\000\001\001\002\001\
\001\001\004\001\005\001\019\001\020\001\021\001\009\001\010\001\
\002\001\003\001\000\000\018\001\000\000\016\001\011\001\017\001\
\012\001\015\001\013\001\022\001\023\001\024\001\014\001\035\000\
\002\001\006\001\040\000\041\000\042\000\003\001\023\001\002\001\
\000\000\003\001\023\001\007\001\002\001\002\001\049\000\051\000\
\008\001\051\000\054\000\057\000\043\000\059\000\001\001\044\000\
\046\000\004\001\005\001\045\000\000\000\048\000\009\001\010\001\
\047\000\255\255\255\255\255\255\255\255\016\001\001\001\075\000\
\076\000\255\255\005\001\022\001\023\001\024\001\009\001\010\001\
\000\000\001\001\255\255\255\255\255\255\005\001\255\255\000\000\
\019\001\009\001\010\001\022\001\023\001\024\001\255\255\255\255\
\255\255\255\255\255\255\019\001\000\000\001\001\022\001\023\001\
\024\001\005\001\255\255\000\000\255\255\009\001\010\001\255\255\
\255\255\255\255\255\255\255\255\016\001\255\255\255\255\255\255\
\000\000\001\001\022\001\023\001\024\001\005\001\255\255\000\000\
\255\255\009\001\010\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\000\000\255\255\022\001\023\001\
\024\001\255\255\255\255\000\000\255\255\255\255\255\255\255\255\
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
\255\255\255\255\002\001\003\001\255\255\255\255\006\001\007\001\
\008\001\255\255\255\255\011\001\012\001\013\001\014\001\255\255\
\255\255\017\001\018\001\019\001\020\001\021\001\002\001\003\001\
\255\255\255\255\006\001\007\001\008\001\255\255\255\255\011\001\
\012\001\013\001\014\001\255\255\255\255\017\001\018\001\019\001\
\020\001\021\001\002\001\003\001\255\255\255\255\006\001\007\001\
\008\001\255\255\255\255\011\001\012\001\013\001\255\255\255\255\
\255\255\017\001\018\001\019\001\020\001\021\001\002\001\003\001\
\255\255\255\255\006\001\007\001\008\001\255\255\255\255\011\001\
\012\001\013\001\255\255\255\255\255\255\017\001\018\001\019\001\
\020\001\021\001\002\001\003\001\255\255\255\255\006\001\007\001\
\008\001\002\001\003\001\011\001\012\001\006\001\007\001\008\001\
\255\255\017\001\018\001\019\001\020\001\021\001\002\001\003\001\
\017\001\018\001\006\001\007\001\008\001\002\001\003\001\011\001\
\012\001\006\001\007\001\008\001\255\255\017\001\018\001\019\001\
\020\001\021\001\002\001\003\001\255\255\018\001\006\001\007\001\
\008\001\002\001\003\001\011\001\255\255\006\001\007\001\008\001\
\255\255\017\001\018\001\019\001\020\001\021\001\002\001\003\001\
\255\255\255\255\006\001\007\001\008\001\002\001\003\001\011\001\
\255\255\006\001\007\001\008\001\255\255\017\001\018\001\019\001\
\020\001\021\001\255\255\255\255\017\001\018\001\019\001\020\001\
\021\001"

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
  GT\000\
  LT\000\
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
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'premain) in
    Obj.repr(
# 51 "a3.mly"
                                      ( _1 )
# 285 "a3.ml"
               : A1.exptree))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 55 "a3.mly"
                                  ( _1 )
# 292 "a3.ml"
               : 'premain))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'tuple) in
    Obj.repr(
# 57 "a3.mly"
                                ( _1 )
# 299 "a3.ml"
               : 'premain))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'projections) in
    Obj.repr(
# 58 "a3.mly"
                                ( _1 )
# 306 "a3.ml"
               : 'premain))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : 'premain) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'premain) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'premain) in
    Obj.repr(
# 62 "a3.mly"
                                               (IfThenElse(_2, _4, _6))
# 315 "a3.ml"
               : 'conditional))
; (fun __caml_parser_env ->
    Obj.repr(
# 67 "a3.mly"
                                  (Tuple(0, []))
# 321 "a3.ml"
               : 'tuple))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'tuple_list) in
    Obj.repr(
# 68 "a3.mly"
                                  ( _2 )
# 328 "a3.ml"
               : 'tuple))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'premain) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'premain) in
    Obj.repr(
# 71 "a3.mly"
                                        (Tuple(2, [_1; _3]))
# 336 "a3.ml"
               : 'tuple_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'premain) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'tuple_list) in
    Obj.repr(
# 72 "a3.mly"
                                     (match _3 with Tuple(x, el) -> Tuple(x+1, _1::el) | _ -> raise Bad_State)
# 344 "a3.ml"
               : 'tuple_list))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : int) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'premain) in
    Obj.repr(
# 76 "a3.mly"
                                     (Project((_3, _5), _7))
# 353 "a3.ml"
               : 'projections))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'or_expression) in
    Obj.repr(
# 80 "a3.mly"
                                              ( _1 )
# 360 "a3.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arithmetic_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arithmetic_expression) in
    Obj.repr(
# 84 "a3.mly"
                                                          (Equals(_1, _3))
# 368 "a3.ml"
               : 'comparison))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arithmetic_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arithmetic_expression) in
    Obj.repr(
# 85 "a3.mly"
                                                          (GreaterT(_1, _3))
# 376 "a3.ml"
               : 'comparison))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arithmetic_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arithmetic_expression) in
    Obj.repr(
# 86 "a3.mly"
                                                          (LessT(_1, _3))
# 384 "a3.ml"
               : 'comparison))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'arithmetic_expression) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'arithmetic_expression) in
    Obj.repr(
# 87 "a3.mly"
                                                          (GreaterTE(_1, _4))
# 392 "a3.ml"
               : 'comparison))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'arithmetic_expression) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'arithmetic_expression) in
    Obj.repr(
# 88 "a3.mly"
                                                          (LessTE(_1, _4))
# 400 "a3.ml"
               : 'comparison))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'and_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'or_expression) in
    Obj.repr(
# 92 "a3.mly"
                                              (Disjunction(_1, _3))
# 408 "a3.ml"
               : 'or_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'and_expression) in
    Obj.repr(
# 93 "a3.mly"
                                              ( _1 )
# 415 "a3.ml"
               : 'or_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'not_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'and_expression) in
    Obj.repr(
# 96 "a3.mly"
                                              (Conjunction(_1, _3))
# 423 "a3.ml"
               : 'and_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'not_expression) in
    Obj.repr(
# 97 "a3.mly"
                                              ( _1 )
# 430 "a3.ml"
               : 'and_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'arithmetic_expression) in
    Obj.repr(
# 100 "a3.mly"
                                              (Not(_2))
# 437 "a3.ml"
               : 'not_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arithmetic_expression) in
    Obj.repr(
# 101 "a3.mly"
                                              ( _1 )
# 444 "a3.ml"
               : 'not_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'comparison) in
    Obj.repr(
# 103 "a3.mly"
                                              ( _1 )
# 451 "a3.ml"
               : 'not_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'add_expression) in
    Obj.repr(
# 107 "a3.mly"
                                              ( _1 )
# 458 "a3.ml"
               : 'arithmetic_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'add_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'sub_expression) in
    Obj.repr(
# 110 "a3.mly"
                                              (Add(_1, _3))
# 466 "a3.ml"
               : 'add_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'sub_expression) in
    Obj.repr(
# 111 "a3.mly"
                                              ( _1 )
# 473 "a3.ml"
               : 'add_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'sub_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'mult_expression) in
    Obj.repr(
# 114 "a3.mly"
                                              (Sub(_1, _3))
# 481 "a3.ml"
               : 'sub_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'mult_expression) in
    Obj.repr(
# 115 "a3.mly"
                                              ( _1 )
# 488 "a3.ml"
               : 'sub_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'mult_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'div_expression) in
    Obj.repr(
# 118 "a3.mly"
                                              (Mult(_1, _3))
# 496 "a3.ml"
               : 'mult_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'div_expression) in
    Obj.repr(
# 119 "a3.mly"
                                              ( _1 )
# 503 "a3.ml"
               : 'mult_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'div_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'rem_expression) in
    Obj.repr(
# 122 "a3.mly"
                                              (Div(_1, _3))
# 511 "a3.ml"
               : 'div_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'rem_expression) in
    Obj.repr(
# 123 "a3.mly"
                                              ( _1 )
# 518 "a3.ml"
               : 'div_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'rem_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'unary_arithmetic) in
    Obj.repr(
# 126 "a3.mly"
                                              (Rem(_1, _3))
# 526 "a3.ml"
               : 'rem_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'unary_arithmetic) in
    Obj.repr(
# 127 "a3.mly"
                                              ( _1 )
# 533 "a3.ml"
               : 'rem_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'basic_unit) in
    Obj.repr(
# 130 "a3.mly"
                                              ( _1 )
# 540 "a3.ml"
               : 'unary_arithmetic))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'unary_arithmetic) in
    Obj.repr(
# 131 "a3.mly"
                                              (Abs(_2))
# 547 "a3.ml"
               : 'unary_arithmetic))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'unary_arithmetic) in
    Obj.repr(
# 132 "a3.mly"
                                              (Negative(_2))
# 554 "a3.ml"
               : 'unary_arithmetic))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 136 "a3.mly"
                                              (N(_1))
# 561 "a3.ml"
               : 'basic_unit))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 138 "a3.mly"
                                              (B(_1))
# 568 "a3.ml"
               : 'basic_unit))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 140 "a3.mly"
                                              (Var(_1))
# 575 "a3.ml"
               : 'basic_unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'premain) in
    Obj.repr(
# 142 "a3.mly"
                                              (InParen(_2))
# 582 "a3.ml"
               : 'basic_unit))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'conditional) in
    Obj.repr(
# 144 "a3.mly"
                                              (_1)
# 589 "a3.ml"
               : 'basic_unit))
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
