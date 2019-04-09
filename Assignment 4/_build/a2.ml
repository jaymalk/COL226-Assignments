# 8 "a2.mll"
 
  open A3
  exception Not_implemented
  exception Lexing_Error

# 8 "a2.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base =
   "\000\000\213\255\214\255\215\255\079\000\098\000\192\000\219\255\
    \220\255\221\255\001\000\000\000\001\000\000\000\002\000\231\255\
    \233\255\000\000\235\255\236\255\007\000\012\000\002\000\240\255\
    \016\000\242\255\243\255\002\000\245\255\018\000\004\000\248\255\
    \249\255\250\255\020\001\002\000\104\001\188\001\016\002\100\002\
    \184\002\012\003\096\003\180\003\008\004\092\004\176\004\004\005\
    \088\005\172\005\000\006\084\006\009\000\001\000\247\255\003\000\
    \023\000\246\255\230\255\244\255\227\255\022\000\239\255\225\255\
    \238\255\024\000\040\000\237\255\040\000\223\255\027\000\234\255\
    \043\000\229\255\028\000\228\255\226\255\044\000\046\000\224\255\
    \222\255";
  Lexing.lex_backtrk =
   "\255\255\255\255\255\255\255\255\040\000\039\000\038\000\255\255\
    \255\255\255\255\042\000\042\000\042\000\042\000\042\000\255\255\
    \255\255\042\000\255\255\255\255\042\000\042\000\042\000\255\255\
    \014\000\255\255\255\255\023\000\255\255\042\000\042\000\255\255\
    \255\255\255\255\037\000\000\000\039\000\039\000\039\000\039\000\
    \039\000\001\000\039\000\039\000\002\000\039\000\039\000\003\000\
    \039\000\039\000\039\000\004\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255";
  Lexing.lex_default =
   "\001\000\000\000\000\000\000\000\255\255\255\255\255\255\000\000\
    \000\000\000\000\255\255\255\255\255\255\255\255\255\255\000\000\
    \000\000\255\255\000\000\000\000\255\255\255\255\255\255\000\000\
    \255\255\000\000\000\000\255\255\000\000\255\255\255\255\000\000\
    \000\000\000\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\000\000\255\255\
    \255\255\000\000\000\000\000\000\000\000\255\255\000\000\000\000\
    \000\000\255\255\255\255\000\000\255\255\000\000\255\255\000\000\
    \255\255\000\000\255\255\000\000\000\000\255\255\255\255\000\000\
    \000\000";
  Lexing.lex_trans =
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\035\000\035\000\035\000\035\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \035\000\000\000\035\000\000\000\019\000\000\000\000\000\000\000\
    \033\000\032\000\015\000\016\000\031\000\027\000\023\000\012\000\
    \003\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\028\000\026\000\007\000\009\000\008\000\060\000\
    \059\000\005\000\005\000\005\000\005\000\005\000\006\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\034\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\000\000\024\000\076\000\000\000\000\000\
    \000\000\017\000\070\000\000\000\029\000\020\000\010\000\061\000\
    \077\000\021\000\080\000\054\000\022\000\014\000\013\000\074\000\
    \030\000\072\000\063\000\065\000\011\000\066\000\052\000\056\000\
    \053\000\058\000\064\000\055\000\025\000\057\000\018\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\005\000\062\000\068\000\067\000\069\000\071\000\073\000\
    \075\000\078\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\079\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\000\000\000\000\000\000\
    \000\000\005\000\000\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\005\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \002\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\000\000\000\000\000\000\000\000\005\000\
    \000\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\000\000\
    \000\000\000\000\000\000\005\000\000\000\005\000\038\000\005\000\
    \005\000\005\000\037\000\005\000\005\000\039\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \036\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\000\000\000\000\000\000\000\000\005\000\
    \000\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\048\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\000\000\
    \000\000\000\000\000\000\005\000\000\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\045\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\000\000\000\000\000\000\000\000\005\000\
    \000\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\042\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\000\000\
    \000\000\000\000\000\000\005\000\000\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\040\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\000\000\000\000\000\000\000\000\005\000\
    \000\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\041\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\000\000\
    \000\000\000\000\000\000\005\000\000\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\000\000\000\000\000\000\000\000\005\000\
    \000\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\043\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\000\000\
    \000\000\000\000\000\000\005\000\000\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \044\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\000\000\000\000\000\000\000\000\005\000\
    \000\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\000\000\
    \000\000\000\000\000\000\005\000\000\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\046\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\000\000\000\000\000\000\000\000\005\000\
    \000\000\005\000\005\000\047\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\000\000\
    \000\000\000\000\000\000\005\000\000\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\000\000\000\000\000\000\000\000\005\000\
    \000\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \049\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\000\000\
    \000\000\000\000\000\000\005\000\000\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \050\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\000\000\000\000\000\000\000\000\005\000\
    \000\000\005\000\005\000\005\000\005\000\051\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\000\000\
    \000\000\000\000\000\000\005\000\000\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\000\000\
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
    \000\000\000\000\000\000\000\000\000\000";
  Lexing.lex_check =
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\035\000\035\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\035\000\255\255\000\000\255\255\255\255\255\255\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\024\000\
    \027\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\255\255\000\000\012\000\255\255\255\255\
    \255\255\000\000\017\000\255\255\000\000\000\000\000\000\022\000\
    \011\000\000\000\010\000\053\000\000\000\000\000\000\000\013\000\
    \000\000\014\000\021\000\020\000\000\000\020\000\030\000\029\000\
    \052\000\055\000\021\000\029\000\000\000\056\000\000\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\005\000\061\000\065\000\066\000\068\000\070\000\072\000\
    \074\000\077\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\078\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\255\255\255\255\255\255\
    \255\255\005\000\255\255\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\006\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
    \006\000\006\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
    \006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
    \006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
    \006\000\006\000\006\000\255\255\255\255\255\255\255\255\006\000\
    \255\255\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
    \006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
    \006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
    \006\000\006\000\006\000\034\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\255\255\
    \255\255\255\255\255\255\034\000\255\255\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\036\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \036\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\255\255\255\255\255\255\255\255\036\000\
    \255\255\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\037\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\037\000\037\000\037\000\037\000\
    \037\000\037\000\037\000\037\000\037\000\037\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\037\000\037\000\037\000\
    \037\000\037\000\037\000\037\000\037\000\037\000\037\000\037\000\
    \037\000\037\000\037\000\037\000\037\000\037\000\037\000\037\000\
    \037\000\037\000\037\000\037\000\037\000\037\000\037\000\255\255\
    \255\255\255\255\255\255\037\000\255\255\037\000\037\000\037\000\
    \037\000\037\000\037\000\037\000\037\000\037\000\037\000\037\000\
    \037\000\037\000\037\000\037\000\037\000\037\000\037\000\037\000\
    \037\000\037\000\037\000\037\000\037\000\037\000\037\000\038\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \038\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
    \038\000\038\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
    \038\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
    \038\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
    \038\000\038\000\038\000\255\255\255\255\255\255\255\255\038\000\
    \255\255\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
    \038\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
    \038\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
    \038\000\038\000\038\000\039\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\039\000\039\000\039\000\039\000\
    \039\000\039\000\039\000\039\000\039\000\039\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\039\000\039\000\039\000\
    \039\000\039\000\039\000\039\000\039\000\039\000\039\000\039\000\
    \039\000\039\000\039\000\039\000\039\000\039\000\039\000\039\000\
    \039\000\039\000\039\000\039\000\039\000\039\000\039\000\255\255\
    \255\255\255\255\255\255\039\000\255\255\039\000\039\000\039\000\
    \039\000\039\000\039\000\039\000\039\000\039\000\039\000\039\000\
    \039\000\039\000\039\000\039\000\039\000\039\000\039\000\039\000\
    \039\000\039\000\039\000\039\000\039\000\039\000\039\000\040\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \040\000\040\000\040\000\040\000\040\000\040\000\040\000\040\000\
    \040\000\040\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\040\000\040\000\040\000\040\000\040\000\040\000\040\000\
    \040\000\040\000\040\000\040\000\040\000\040\000\040\000\040\000\
    \040\000\040\000\040\000\040\000\040\000\040\000\040\000\040\000\
    \040\000\040\000\040\000\255\255\255\255\255\255\255\255\040\000\
    \255\255\040\000\040\000\040\000\040\000\040\000\040\000\040\000\
    \040\000\040\000\040\000\040\000\040\000\040\000\040\000\040\000\
    \040\000\040\000\040\000\040\000\040\000\040\000\040\000\040\000\
    \040\000\040\000\040\000\041\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\041\000\041\000\041\000\041\000\
    \041\000\041\000\041\000\041\000\041\000\041\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\041\000\041\000\041\000\
    \041\000\041\000\041\000\041\000\041\000\041\000\041\000\041\000\
    \041\000\041\000\041\000\041\000\041\000\041\000\041\000\041\000\
    \041\000\041\000\041\000\041\000\041\000\041\000\041\000\255\255\
    \255\255\255\255\255\255\041\000\255\255\041\000\041\000\041\000\
    \041\000\041\000\041\000\041\000\041\000\041\000\041\000\041\000\
    \041\000\041\000\041\000\041\000\041\000\041\000\041\000\041\000\
    \041\000\041\000\041\000\041\000\041\000\041\000\041\000\042\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \042\000\042\000\042\000\042\000\042\000\042\000\042\000\042\000\
    \042\000\042\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\042\000\042\000\042\000\042\000\042\000\042\000\042\000\
    \042\000\042\000\042\000\042\000\042\000\042\000\042\000\042\000\
    \042\000\042\000\042\000\042\000\042\000\042\000\042\000\042\000\
    \042\000\042\000\042\000\255\255\255\255\255\255\255\255\042\000\
    \255\255\042\000\042\000\042\000\042\000\042\000\042\000\042\000\
    \042\000\042\000\042\000\042\000\042\000\042\000\042\000\042\000\
    \042\000\042\000\042\000\042\000\042\000\042\000\042\000\042\000\
    \042\000\042\000\042\000\043\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\043\000\043\000\043\000\043\000\
    \043\000\043\000\043\000\043\000\043\000\043\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\043\000\043\000\043\000\
    \043\000\043\000\043\000\043\000\043\000\043\000\043\000\043\000\
    \043\000\043\000\043\000\043\000\043\000\043\000\043\000\043\000\
    \043\000\043\000\043\000\043\000\043\000\043\000\043\000\255\255\
    \255\255\255\255\255\255\043\000\255\255\043\000\043\000\043\000\
    \043\000\043\000\043\000\043\000\043\000\043\000\043\000\043\000\
    \043\000\043\000\043\000\043\000\043\000\043\000\043\000\043\000\
    \043\000\043\000\043\000\043\000\043\000\043\000\043\000\044\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \044\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\255\255\255\255\255\255\255\255\044\000\
    \255\255\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\045\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\045\000\045\000\045\000\045\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\045\000\045\000\045\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\255\255\
    \255\255\255\255\255\255\045\000\255\255\045\000\045\000\045\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\046\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \046\000\046\000\046\000\046\000\046\000\046\000\046\000\046\000\
    \046\000\046\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\046\000\046\000\046\000\046\000\046\000\046\000\046\000\
    \046\000\046\000\046\000\046\000\046\000\046\000\046\000\046\000\
    \046\000\046\000\046\000\046\000\046\000\046\000\046\000\046\000\
    \046\000\046\000\046\000\255\255\255\255\255\255\255\255\046\000\
    \255\255\046\000\046\000\046\000\046\000\046\000\046\000\046\000\
    \046\000\046\000\046\000\046\000\046\000\046\000\046\000\046\000\
    \046\000\046\000\046\000\046\000\046\000\046\000\046\000\046\000\
    \046\000\046\000\046\000\047\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\255\255\
    \255\255\255\255\255\255\047\000\255\255\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\048\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \048\000\048\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \048\000\048\000\048\000\255\255\255\255\255\255\255\255\048\000\
    \255\255\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \048\000\048\000\048\000\049\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\049\000\049\000\049\000\049\000\
    \049\000\049\000\049\000\049\000\049\000\049\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\049\000\049\000\049\000\
    \049\000\049\000\049\000\049\000\049\000\049\000\049\000\049\000\
    \049\000\049\000\049\000\049\000\049\000\049\000\049\000\049\000\
    \049\000\049\000\049\000\049\000\049\000\049\000\049\000\255\255\
    \255\255\255\255\255\255\049\000\255\255\049\000\049\000\049\000\
    \049\000\049\000\049\000\049\000\049\000\049\000\049\000\049\000\
    \049\000\049\000\049\000\049\000\049\000\049\000\049\000\049\000\
    \049\000\049\000\049\000\049\000\049\000\049\000\049\000\050\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \050\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
    \050\000\050\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
    \050\000\050\000\050\000\255\255\255\255\255\255\255\255\050\000\
    \255\255\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
    \050\000\050\000\050\000\051\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\255\255\
    \255\255\255\255\255\255\051\000\255\255\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\255\255\
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
    \255\255\255\255\255\255\255\255\255\255";
  Lexing.lex_base_code =
   "";
  Lexing.lex_backtrk_code =
   "";
  Lexing.lex_default_code =
   "";
  Lexing.lex_trans_code =
   "";
  Lexing.lex_check_code =
   "";
  Lexing.lex_code =
   "";
}

let rec read lexbuf =
   __ocaml_lex_read_rec lexbuf 0
and __ocaml_lex_read_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 25 "a2.mll"
                        ( read lexbuf  (* Ignoring white space. *))
# 539 "a2.ml"

  | 1 ->
# 28 "a2.mll"
                        ( TT )
# 544 "a2.ml"

  | 2 ->
# 29 "a2.mll"
                        ( TB )
# 549 "a2.ml"

  | 3 ->
# 30 "a2.mll"
                        ( TF )
# 554 "a2.ml"

  | 4 ->
# 31 "a2.mll"
                        ( TP )
# 559 "a2.ml"

  | 5 ->
# 33 "a2.mll"
                        ( LP        (* Left Parathesis *))
# 564 "a2.ml"

  | 6 ->
# 34 "a2.mll"
                        ( RP        (* Right Parathesis *))
# 569 "a2.ml"

  | 7 ->
# 36 "a2.mll"
                        ( COMMA     (* Seperator for N-Tuples *))
# 574 "a2.ml"

  | 8 ->
# 37 "a2.mll"
                        ( PROJ      (* Projector Syntax for N-Tuples *))
# 579 "a2.ml"

  | 9 ->
# 39 "a2.mll"
                        ( DEF       (* Definitional Keyword *))
# 584 "a2.ml"

  | 10 ->
# 40 "a2.mll"
                        ( COLON )
# 589 "a2.ml"

  | 11 ->
# 41 "a2.mll"
                        ( ARROW )
# 594 "a2.ml"

  | 12 ->
# 42 "a2.mll"
                        ( SEMICOLON )
# 599 "a2.ml"

  | 13 ->
# 43 "a2.mll"
                        ( PARALLEL )
# 604 "a2.ml"

  | 14 ->
# 44 "a2.mll"
                        ( BACKSLASH )
# 609 "a2.ml"

  | 15 ->
# 45 "a2.mll"
                        ( DOT )
# 614 "a2.ml"

  | 16 ->
# 47 "a2.mll"
                        ( LET )
# 619 "a2.ml"

  | 17 ->
# 48 "a2.mll"
                        ( IN    (* Local definitions in expressions *))
# 624 "a2.ml"

  | 18 ->
# 49 "a2.mll"
                        ( END )
# 629 "a2.ml"

  | 19 ->
# 51 "a2.mll"
                        ( DELIMITER (* Command Delimiter *))
# 634 "a2.ml"

  | 20 ->
# 53 "a2.mll"
                        ( TILDA     (* Unary Minus *))
# 639 "a2.ml"

  | 21 ->
# 54 "a2.mll"
                        ( ABS       (* Absolute Value *))
# 644 "a2.ml"

  | 22 ->
# 56 "a2.mll"
                        ( PLUS )
# 649 "a2.ml"

  | 23 ->
# 57 "a2.mll"
                        ( MINUS )
# 654 "a2.ml"

  | 24 ->
# 58 "a2.mll"
                        ( TIMES     (* Binary Arithmetic Operation Keywords *))
# 659 "a2.ml"

  | 25 ->
# 59 "a2.mll"
                        ( DIV )
# 664 "a2.ml"

  | 26 ->
# 60 "a2.mll"
                        ( REM )
# 669 "a2.ml"

  | 27 ->
# 62 "a2.mll"
                        ( NOT       (* Logical Negation *))
# 674 "a2.ml"

  | 28 ->
# 64 "a2.mll"
                        ( DISJ      (* Binary Boolean Operation Keywords *))
# 679 "a2.ml"

  | 29 ->
# 65 "a2.mll"
                        ( CONJ )
# 684 "a2.ml"

  | 30 ->
# 67 "a2.mll"
                        ( IF )
# 689 "a2.ml"

  | 31 ->
# 68 "a2.mll"
                        ( THEN      (* Conditional Statement Keywords *))
# 694 "a2.ml"

  | 32 ->
# 69 "a2.mll"
                        ( ELSE )
# 699 "a2.ml"

  | 33 ->
# 70 "a2.mll"
                        ( FI )
# 704 "a2.ml"

  | 34 ->
# 72 "a2.mll"
                        ( EQ )
# 709 "a2.ml"

  | 35 ->
# 73 "a2.mll"
                        ( GT        (* Comparison Keywords *))
# 714 "a2.ml"

  | 36 ->
# 74 "a2.mll"
                        ( LT )
# 719 "a2.ml"

  | 37 ->
# 76 "a2.mll"
                        ( BOOL(true) )
# 724 "a2.ml"

  | 38 ->
# 77 "a2.mll"
                        ( BOOL(false)  (* Boolean constants *))
# 729 "a2.ml"

  | 39 ->
let
# 79 "a2.mll"
          i
# 735 "a2.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 79 "a2.mll"
                        ( ID (i)      (* Variable IDs *))
# 739 "a2.ml"

  | 40 ->
let
# 82 "a2.mll"
             i
# 745 "a2.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 82 "a2.mll"
                        ((INT(int_of_string i)) (* INTEGER TYPE *))
# 749 "a2.ml"

  | 41 ->
# 84 "a2.mll"
                        ( EOF          (* End of file marker *))
# 754 "a2.ml"

  | 42 ->
# 86 "a2.mll"
                        ( raise Lexing_Error )
# 759 "a2.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_rec lexbuf __ocaml_lex_state

;;
