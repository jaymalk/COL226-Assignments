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
  | TU
  | DEF
  | LET
  | IN
  | END
  | LOCAL
  | SEMICOLON
  | PARALLEL
  | DELIMITER
  | EOF

open Parsing;;
let _ = parse_error;;
# 2 "a3.mly"
    open A1
    exception Bad_State
# 51 "a3.ml"
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
  278 (* BACKSLASH *);
  279 (* DOT *);
  280 (* COLON *);
  281 (* ARROW *);
  285 (* TT *);
  286 (* TB *);
  287 (* TP *);
  288 (* TF *);
  289 (* TU *);
  290 (* DEF *);
  291 (* LET *);
  292 (* IN *);
  293 (* END *);
  294 (* LOCAL *);
  295 (* SEMICOLON *);
  296 (* PARALLEL *);
  297 (* DELIMITER *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  282 (* BOOL *);
  283 (* INT *);
  284 (* ID *);
    0|]

let yylhs = "\255\255\
\002\000\004\000\005\000\007\000\007\000\007\000\007\000\007\000\
\006\000\006\000\009\000\009\000\010\000\010\000\010\000\008\000\
\011\000\011\000\012\000\012\000\013\000\013\000\014\000\014\000\
\015\000\015\000\016\000\016\000\016\000\017\000\017\000\018\000\
\018\000\019\000\019\000\020\000\020\000\021\000\021\000\021\000\
\022\000\022\000\023\000\023\000\023\000\023\000\023\000\001\000\
\024\000\024\000\025\000\025\000\025\000\026\000\003\000\003\000\
\027\000\027\000\028\000\028\000\029\000\029\000\029\000\029\000\
\000\000\000\000\000\000"

let yylen = "\002\000\
\002\000\001\000\001\000\003\000\003\000\003\000\004\000\004\000\
\003\000\001\000\003\000\001\000\002\000\001\000\001\000\001\000\
\003\000\001\000\003\000\001\000\003\000\001\000\003\000\001\000\
\003\000\001\000\001\000\002\000\002\000\007\000\001\000\007\000\
\001\000\002\000\001\000\006\000\001\000\002\000\003\000\001\000\
\003\000\003\000\001\000\001\000\001\000\003\000\005\000\002\000\
\005\000\001\000\003\000\004\000\001\000\006\000\003\000\001\000\
\001\000\001\000\003\000\003\000\001\000\001\000\001\000\003\000\
\002\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\065\000\000\000\
\000\000\053\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\044\000\043\000\045\000\000\000\066\000\000\000\002\000\
\003\000\015\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\026\000\027\000\031\000\000\000\035\000\037\000\
\040\000\000\000\061\000\062\000\063\000\067\000\000\000\057\000\
\000\000\000\000\000\000\048\000\000\000\000\000\038\000\000\000\
\000\000\000\000\000\000\029\000\028\000\013\000\000\000\000\000\
\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\034\000\000\000\000\000\000\000\000\000\
\000\000\051\000\000\000\046\000\000\000\039\000\000\000\000\000\
\000\000\000\000\004\000\000\000\005\000\000\000\006\000\009\000\
\011\000\000\000\000\000\000\000\000\000\025\000\064\000\055\000\
\060\000\000\000\000\000\000\000\052\000\000\000\042\000\000\000\
\000\000\000\000\000\000\007\000\008\000\000\000\049\000\000\000\
\000\000\000\000\047\000\054\000\000\000\000\000\036\000\030\000\
\032\000"

let yydgoto = "\004\000\
\007\000\022\000\046\000\023\000\024\000\025\000\026\000\027\000\
\028\000\029\000\030\000\031\000\032\000\033\000\034\000\035\000\
\036\000\037\000\038\000\039\000\040\000\057\000\041\000\008\000\
\009\000\010\000\047\000\048\000\049\000"

let yysindex = "\014\000\
\236\254\093\255\030\255\000\000\232\254\236\254\000\000\007\000\
\253\254\000\000\019\255\008\255\093\255\107\255\107\255\093\255\
\247\254\000\000\000\000\000\000\236\254\000\000\022\000\000\000\
\000\000\000\000\068\255\009\255\026\255\041\255\046\255\048\255\
\053\255\055\255\000\000\000\000\000\000\124\255\000\000\000\000\
\000\000\030\255\000\000\000\000\000\000\000\000\037\255\000\000\
\058\255\051\255\040\255\000\000\044\255\056\255\000\000\047\255\
\079\255\066\255\089\255\000\000\000\000\000\000\076\255\065\255\
\000\000\107\255\029\255\064\255\093\255\093\255\107\255\107\255\
\107\255\107\255\107\255\000\000\102\255\030\255\030\255\030\255\
\236\254\000\000\044\255\000\000\093\255\000\000\103\255\093\255\
\030\255\093\255\000\000\107\255\000\000\107\255\000\000\000\000\
\000\000\046\255\048\255\053\255\055\255\000\000\000\000\000\000\
\000\000\058\255\088\255\077\255\000\000\119\255\000\000\096\255\
\117\255\104\255\094\255\000\000\000\000\093\255\000\000\128\255\
\093\255\124\255\000\000\000\000\136\255\118\255\000\000\000\000\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\013\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\066\000\105\000\026\000\055\001\006\001\196\000\
\118\000\040\000\000\000\000\000\000\000\001\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\008\000\000\000\
\148\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\030\001\235\000\157\000\079\000\000\000\000\000\000\000\
\000\000\160\000\000\000\000\000\000\000\130\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\000\000\000\000\220\255\248\255\000\000\067\000\000\000\087\000\
\068\000\123\000\000\000\072\000\073\000\071\000\075\000\253\255\
\031\000\000\000\000\000\218\255\000\000\062\000\000\000\004\000\
\000\000\205\255\000\000\082\000\086\000"

let yytablesize = 607
let yytable = "\076\000\
\033\000\082\000\056\000\050\000\059\000\077\000\052\000\056\000\
\058\000\051\000\060\000\061\000\050\000\005\000\001\000\002\000\
\003\000\006\000\063\000\011\000\055\000\065\000\012\000\013\000\
\064\000\012\000\069\000\014\000\015\000\011\000\042\000\109\000\
\012\000\013\000\016\000\053\000\054\000\014\000\015\000\024\000\
\017\000\104\000\070\000\107\000\018\000\019\000\020\000\092\000\
\084\000\085\000\017\000\071\000\114\000\021\000\018\000\019\000\
\020\000\072\000\043\000\044\000\073\000\078\000\045\000\021\000\
\011\000\014\000\074\000\012\000\013\000\075\000\079\000\102\000\
\014\000\015\000\080\000\081\000\110\000\005\000\023\000\113\000\
\086\000\115\000\094\000\127\000\108\000\017\000\066\000\067\000\
\068\000\018\000\019\000\020\000\087\000\011\000\088\000\083\000\
\012\000\013\000\021\000\089\000\090\000\014\000\015\000\103\000\
\010\000\112\000\118\000\011\000\016\000\124\000\012\000\013\000\
\126\000\119\000\017\000\014\000\015\000\022\000\018\000\019\000\
\020\000\085\000\120\000\121\000\011\000\129\000\122\000\021\000\
\017\000\125\000\123\000\041\000\018\000\019\000\020\000\096\000\
\011\000\097\000\062\000\012\000\013\000\021\000\098\000\100\000\
\099\000\017\000\111\000\058\000\101\000\018\000\019\000\020\000\
\091\000\093\000\095\000\128\000\021\000\017\000\021\000\059\000\
\105\000\018\000\019\000\020\000\106\000\000\000\000\000\000\000\
\000\000\000\000\021\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\116\000\000\000\117\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\020\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\019\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\033\000\033\000\000\000\018\000\033\000\033\000\
\033\000\056\000\000\000\033\000\033\000\033\000\033\000\033\000\
\000\000\033\000\033\000\033\000\033\000\033\000\000\000\000\000\
\000\000\000\000\056\000\012\000\012\000\017\000\056\000\012\000\
\012\000\012\000\000\000\000\000\033\000\033\000\000\000\033\000\
\033\000\024\000\024\000\012\000\000\000\024\000\024\000\024\000\
\050\000\050\000\024\000\024\000\024\000\024\000\016\000\000\000\
\024\000\024\000\024\000\024\000\024\000\012\000\012\000\000\000\
\012\000\012\000\000\000\014\000\014\000\000\000\000\000\014\000\
\014\000\014\000\000\000\024\000\024\000\000\000\024\000\024\000\
\023\000\023\000\014\000\014\000\023\000\023\000\023\000\000\000\
\000\000\023\000\023\000\023\000\023\000\000\000\000\000\023\000\
\023\000\023\000\023\000\023\000\000\000\014\000\014\000\000\000\
\014\000\014\000\010\000\010\000\000\000\000\000\010\000\010\000\
\010\000\000\000\023\000\023\000\000\000\023\000\023\000\022\000\
\022\000\000\000\000\000\022\000\022\000\022\000\000\000\000\000\
\022\000\022\000\022\000\000\000\000\000\000\000\022\000\022\000\
\022\000\022\000\022\000\000\000\010\000\010\000\000\000\010\000\
\010\000\000\000\000\000\000\000\000\000\058\000\000\000\000\000\
\000\000\022\000\022\000\000\000\022\000\022\000\021\000\021\000\
\000\000\059\000\021\000\021\000\021\000\000\000\058\000\021\000\
\021\000\021\000\058\000\000\000\058\000\021\000\021\000\021\000\
\021\000\021\000\059\000\000\000\000\000\000\000\059\000\000\000\
\059\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\021\000\021\000\000\000\021\000\021\000\020\000\020\000\000\000\
\000\000\020\000\020\000\020\000\000\000\000\000\020\000\020\000\
\000\000\000\000\000\000\000\000\020\000\020\000\020\000\020\000\
\020\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\020\000\
\020\000\000\000\020\000\020\000\019\000\019\000\000\000\000\000\
\019\000\019\000\019\000\000\000\000\000\019\000\019\000\000\000\
\000\000\000\000\000\000\019\000\019\000\019\000\019\000\019\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\018\000\
\018\000\000\000\000\000\018\000\018\000\018\000\019\000\019\000\
\018\000\019\000\019\000\000\000\000\000\000\000\018\000\018\000\
\018\000\018\000\018\000\000\000\000\000\000\000\000\000\017\000\
\017\000\000\000\000\000\017\000\017\000\017\000\000\000\000\000\
\017\000\018\000\018\000\000\000\018\000\018\000\017\000\017\000\
\017\000\017\000\017\000\000\000\000\000\000\000\000\000\000\000\
\016\000\016\000\000\000\000\000\016\000\016\000\016\000\000\000\
\000\000\017\000\017\000\000\000\017\000\017\000\000\000\016\000\
\016\000\016\000\016\000\016\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\016\000\016\000\000\000\016\000\016\000"

let yycheck = "\038\000\
\000\000\053\000\011\000\028\001\013\000\042\000\000\000\000\000\
\001\001\006\000\014\000\015\000\000\000\034\001\001\000\002\000\
\003\000\038\001\028\001\001\001\002\001\000\000\004\001\005\001\
\021\000\000\000\018\001\009\001\010\001\001\001\001\001\083\000\
\004\001\005\001\016\001\039\001\040\001\009\001\010\001\000\000\
\022\001\078\000\017\001\080\000\026\001\027\001\028\001\019\001\
\002\001\003\001\022\001\011\001\089\000\035\001\026\001\027\001\
\028\001\012\001\029\001\030\001\013\001\025\001\033\001\035\001\
\001\001\000\000\014\001\004\001\005\001\015\001\013\001\075\000\
\009\001\010\001\024\001\036\001\085\000\034\001\000\000\088\000\
\002\001\090\000\019\001\122\000\081\000\022\001\019\001\020\001\
\021\001\026\001\027\001\028\001\027\001\001\001\006\001\040\001\
\004\001\005\001\035\001\024\001\036\001\009\001\010\001\002\001\
\000\000\003\001\019\001\001\001\016\001\118\000\004\001\005\001\
\121\000\037\001\022\001\009\001\010\001\000\000\026\001\027\001\
\028\001\003\001\027\001\007\001\001\001\008\001\023\001\035\001\
\022\001\002\001\037\001\002\001\026\001\027\001\028\001\069\000\
\001\001\070\000\016\000\004\001\005\001\035\001\071\000\073\000\
\072\000\022\001\085\000\000\000\074\000\026\001\027\001\028\001\
\066\000\067\000\068\000\125\000\000\000\022\001\035\001\000\000\
\079\000\026\001\027\001\028\001\079\000\255\255\255\255\255\255\
\255\255\255\255\035\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\092\000\255\255\094\000\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\000\000\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\000\000\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\002\001\003\001\255\255\000\000\006\001\007\001\
\008\001\002\001\255\255\011\001\012\001\013\001\014\001\015\001\
\255\255\017\001\018\001\019\001\020\001\021\001\255\255\255\255\
\255\255\255\255\019\001\002\001\003\001\000\000\023\001\006\001\
\007\001\008\001\255\255\255\255\036\001\037\001\255\255\039\001\
\040\001\002\001\003\001\018\001\255\255\006\001\007\001\008\001\
\036\001\037\001\011\001\012\001\013\001\014\001\000\000\255\255\
\017\001\018\001\019\001\020\001\021\001\036\001\037\001\255\255\
\039\001\040\001\255\255\002\001\003\001\255\255\255\255\006\001\
\007\001\008\001\255\255\036\001\037\001\255\255\039\001\040\001\
\002\001\003\001\017\001\018\001\006\001\007\001\008\001\255\255\
\255\255\011\001\012\001\013\001\014\001\255\255\255\255\017\001\
\018\001\019\001\020\001\021\001\255\255\036\001\037\001\255\255\
\039\001\040\001\002\001\003\001\255\255\255\255\006\001\007\001\
\008\001\255\255\036\001\037\001\255\255\039\001\040\001\002\001\
\003\001\255\255\255\255\006\001\007\001\008\001\255\255\255\255\
\011\001\012\001\013\001\255\255\255\255\255\255\017\001\018\001\
\019\001\020\001\021\001\255\255\036\001\037\001\255\255\039\001\
\040\001\255\255\255\255\255\255\255\255\002\001\255\255\255\255\
\255\255\036\001\037\001\255\255\039\001\040\001\002\001\003\001\
\255\255\002\001\006\001\007\001\008\001\255\255\019\001\011\001\
\012\001\013\001\023\001\255\255\025\001\017\001\018\001\019\001\
\020\001\021\001\019\001\255\255\255\255\255\255\023\001\255\255\
\025\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\036\001\037\001\255\255\039\001\040\001\002\001\003\001\255\255\
\255\255\006\001\007\001\008\001\255\255\255\255\011\001\012\001\
\255\255\255\255\255\255\255\255\017\001\018\001\019\001\020\001\
\021\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\036\001\
\037\001\255\255\039\001\040\001\002\001\003\001\255\255\255\255\
\006\001\007\001\008\001\255\255\255\255\011\001\012\001\255\255\
\255\255\255\255\255\255\017\001\018\001\019\001\020\001\021\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\002\001\
\003\001\255\255\255\255\006\001\007\001\008\001\036\001\037\001\
\011\001\039\001\040\001\255\255\255\255\255\255\017\001\018\001\
\019\001\020\001\021\001\255\255\255\255\255\255\255\255\002\001\
\003\001\255\255\255\255\006\001\007\001\008\001\255\255\255\255\
\011\001\036\001\037\001\255\255\039\001\040\001\017\001\018\001\
\019\001\020\001\021\001\255\255\255\255\255\255\255\255\255\255\
\002\001\003\001\255\255\255\255\006\001\007\001\008\001\255\255\
\255\255\036\001\037\001\255\255\039\001\040\001\255\255\017\001\
\018\001\019\001\020\001\021\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\036\001\037\001\255\255\039\001\040\001"

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
  BACKSLASH\000\
  DOT\000\
  COLON\000\
  ARROW\000\
  TT\000\
  TB\000\
  TP\000\
  TF\000\
  TU\000\
  DEF\000\
  LET\000\
  IN\000\
  END\000\
  LOCAL\000\
  SEMICOLON\000\
  PARALLEL\000\
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
# 58 "a3.mly"
                                    ( _1 )
# 403 "a3.ml"
               : A1.exptree))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 62 "a3.mly"
                                  ( _1 )
# 410 "a3.ml"
               : 'premain))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'or_expression) in
    Obj.repr(
# 77 "a3.mly"
                                              ( _1 )
# 417 "a3.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arithmetic_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arithmetic_expression) in
    Obj.repr(
# 81 "a3.mly"
                                                          (Equals(_1, _3))
# 425 "a3.ml"
               : 'comparison))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arithmetic_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arithmetic_expression) in
    Obj.repr(
# 82 "a3.mly"
                                                          (GreaterT(_1, _3))
# 433 "a3.ml"
               : 'comparison))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arithmetic_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arithmetic_expression) in
    Obj.repr(
# 83 "a3.mly"
                                                          (LessT(_1, _3))
# 441 "a3.ml"
               : 'comparison))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'arithmetic_expression) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'arithmetic_expression) in
    Obj.repr(
# 84 "a3.mly"
                                                          (GreaterTE(_1, _4))
# 449 "a3.ml"
               : 'comparison))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'arithmetic_expression) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'arithmetic_expression) in
    Obj.repr(
# 85 "a3.mly"
                                                          (LessTE(_1, _4))
# 457 "a3.ml"
               : 'comparison))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'and_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'or_expression) in
    Obj.repr(
# 89 "a3.mly"
                                              (Disjunction(_1, _3))
# 465 "a3.ml"
               : 'or_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'and_expression) in
    Obj.repr(
# 90 "a3.mly"
                                              ( _1 )
# 472 "a3.ml"
               : 'or_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'not_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'and_expression) in
    Obj.repr(
# 93 "a3.mly"
                                              (Conjunction(_1, _3))
# 480 "a3.ml"
               : 'and_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'not_expression) in
    Obj.repr(
# 94 "a3.mly"
                                              ( _1 )
# 487 "a3.ml"
               : 'and_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'not_expression) in
    Obj.repr(
# 97 "a3.mly"
                                              (Not(_2))
# 494 "a3.ml"
               : 'not_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arithmetic_expression) in
    Obj.repr(
# 98 "a3.mly"
                                              ( _1 )
# 501 "a3.ml"
               : 'not_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'comparison) in
    Obj.repr(
# 100 "a3.mly"
                                              ( _1 )
# 508 "a3.ml"
               : 'not_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'add_expression) in
    Obj.repr(
# 104 "a3.mly"
                                              ( _1 )
# 515 "a3.ml"
               : 'arithmetic_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'add_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'sub_expression) in
    Obj.repr(
# 107 "a3.mly"
                                              (Add(_1, _3))
# 523 "a3.ml"
               : 'add_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'sub_expression) in
    Obj.repr(
# 108 "a3.mly"
                                              ( _1 )
# 530 "a3.ml"
               : 'add_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'sub_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'mult_expression) in
    Obj.repr(
# 111 "a3.mly"
                                              (Sub(_1, _3))
# 538 "a3.ml"
               : 'sub_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'mult_expression) in
    Obj.repr(
# 112 "a3.mly"
                                              ( _1 )
# 545 "a3.ml"
               : 'sub_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'mult_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'div_expression) in
    Obj.repr(
# 115 "a3.mly"
                                              (Mult(_1, _3))
# 553 "a3.ml"
               : 'mult_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'div_expression) in
    Obj.repr(
# 116 "a3.mly"
                                              ( _1 )
# 560 "a3.ml"
               : 'mult_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'div_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'rem_expression) in
    Obj.repr(
# 119 "a3.mly"
                                              (Div(_1, _3))
# 568 "a3.ml"
               : 'div_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'rem_expression) in
    Obj.repr(
# 120 "a3.mly"
                                              ( _1 )
# 575 "a3.ml"
               : 'div_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'rem_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'unary_arithmetic) in
    Obj.repr(
# 123 "a3.mly"
                                              (Rem(_1, _3))
# 583 "a3.ml"
               : 'rem_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'unary_arithmetic) in
    Obj.repr(
# 124 "a3.mly"
                                              ( _1 )
# 590 "a3.ml"
               : 'rem_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'projections) in
    Obj.repr(
# 127 "a3.mly"
                                              ( _1 )
# 597 "a3.ml"
               : 'unary_arithmetic))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'unary_arithmetic) in
    Obj.repr(
# 128 "a3.mly"
                                              (Abs(_2))
# 604 "a3.ml"
               : 'unary_arithmetic))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'unary_arithmetic) in
    Obj.repr(
# 129 "a3.mly"
                                              (Negative(_2))
# 611 "a3.ml"
               : 'unary_arithmetic))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : int) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'projections) in
    Obj.repr(
# 133 "a3.mly"
                                              (Project((_3, _5), _7))
# 620 "a3.ml"
               : 'projections))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'conditional) in
    Obj.repr(
# 134 "a3.mly"
                                              ( _1 )
# 627 "a3.ml"
               : 'projections))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : 'premain) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'premain) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'premain) in
    Obj.repr(
# 138 "a3.mly"
                                                 (IfThenElse(_2, _4, _6))
# 636 "a3.ml"
               : 'conditional))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'func_call) in
    Obj.repr(
# 139 "a3.mly"
                                                 ( _1 )
# 643 "a3.ml"
               : 'conditional))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'func_call) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'func_abs) in
    Obj.repr(
# 144 "a3.mly"
                                    (FunctionCall(_1, _2))
# 651 "a3.ml"
               : 'func_call))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'func_abs) in
    Obj.repr(
# 145 "a3.mly"
                                    ( _1 )
# 658 "a3.ml"
               : 'func_call))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : A1.exptype) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'func_abs) in
    Obj.repr(
# 148 "a3.mly"
                                                     (FunctionAbstractionType(_2, _4, _6))
# 667 "a3.ml"
               : 'func_abs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'tuple) in
    Obj.repr(
# 149 "a3.mly"
                                                     ( _1 )
# 674 "a3.ml"
               : 'func_abs))
; (fun __caml_parser_env ->
    Obj.repr(
# 154 "a3.mly"
                                  (Tuple(0, []))
# 680 "a3.ml"
               : 'tuple))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'tuple_list) in
    Obj.repr(
# 155 "a3.mly"
                                  ( _2 )
# 687 "a3.ml"
               : 'tuple))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'basic_unit) in
    Obj.repr(
# 156 "a3.mly"
                                  ( _1 )
# 694 "a3.ml"
               : 'tuple))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'premain) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'premain) in
    Obj.repr(
# 159 "a3.mly"
                                        (Tuple(2, [_1; _3]))
# 702 "a3.ml"
               : 'tuple_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'premain) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'tuple_list) in
    Obj.repr(
# 160 "a3.mly"
                                        (match _3 with Tuple(x, el) -> Tuple(x+1, _1::el) | _ -> raise Bad_State)
# 710 "a3.ml"
               : 'tuple_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 165 "a3.mly"
                                              (N(_1))
# 717 "a3.ml"
               : 'basic_unit))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 167 "a3.mly"
                                              (B(_1))
# 724 "a3.ml"
               : 'basic_unit))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 169 "a3.mly"
                                              (Var(_1))
# 731 "a3.ml"
               : 'basic_unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'premain) in
    Obj.repr(
# 171 "a3.mly"
                                              (InParen(_2))
# 738 "a3.ml"
               : 'basic_unit))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'predef) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'premain) in
    Obj.repr(
# 173 "a3.mly"
                                              (Let(_2, _4))
# 746 "a3.ml"
               : 'basic_unit))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'predef) in
    Obj.repr(
# 177 "a3.mly"
                                           (_1)
# 753 "a3.ml"
               : A1.definition))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'predef) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'predef) in
    Obj.repr(
# 181 "a3.mly"
                                   (Local(_2, _4))
# 761 "a3.ml"
               : 'predef))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'series) in
    Obj.repr(
# 182 "a3.mly"
                                   ( _1 )
# 768 "a3.ml"
               : 'predef))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'series) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'basic_def) in
    Obj.repr(
# 186 "a3.mly"
                                               (Sequence([_1; _3]))
# 776 "a3.ml"
               : 'series))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'series) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'basic_def) in
    Obj.repr(
# 187 "a3.mly"
                                               (Parallel([_1; _4]))
# 784 "a3.ml"
               : 'series))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'basic_def) in
    Obj.repr(
# 188 "a3.mly"
                                               ( _1 )
# 791 "a3.ml"
               : 'series))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : A1.exptype) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'premain) in
    Obj.repr(
# 192 "a3.mly"
                                               (SimpleType(_2, _4, _6))
# 800 "a3.ml"
               : 'basic_def))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'tuple_type) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : A1.exptype) in
    Obj.repr(
# 197 "a3.mly"
                                            (Tfunc(_1, _3))
# 808 "a3.ml"
               : A1.exptype))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'tuple_type) in
    Obj.repr(
# 198 "a3.mly"
                                            ( _1 )
# 815 "a3.ml"
               : A1.exptype))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'tuple_type_list) in
    Obj.repr(
# 201 "a3.mly"
                                            (_1)
# 822 "a3.ml"
               : 'tuple_type))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'basic_type) in
    Obj.repr(
# 202 "a3.mly"
                                            (_1)
# 829 "a3.ml"
               : 'tuple_type))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'basic_type) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'basic_type) in
    Obj.repr(
# 205 "a3.mly"
                                            (Ttuple([_1; _3]))
# 837 "a3.ml"
               : 'tuple_type_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'basic_type) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'tuple_type_list) in
    Obj.repr(
# 206 "a3.mly"
                                            (match _3 with Ttuple(tl) -> Ttuple(_1::tl) | _ -> raise Bad_State)
# 845 "a3.ml"
               : 'tuple_type_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 209 "a3.mly"
                                            (Tint)
# 851 "a3.ml"
               : 'basic_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 210 "a3.mly"
                                            (Tbool)
# 857 "a3.ml"
               : 'basic_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 211 "a3.mly"
                                            (Tunit)
# 863 "a3.ml"
               : 'basic_type))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : A1.exptype) in
    Obj.repr(
# 212 "a3.mly"
                                            (_2)
# 870 "a3.ml"
               : 'basic_type))
(* Entry def_parser *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry exp_parser *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry type_parser *)
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
let def_parser (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : A1.definition)
let exp_parser (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 2 lexfun lexbuf : A1.exptree)
let type_parser (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 3 lexfun lexbuf : A1.exptype)
