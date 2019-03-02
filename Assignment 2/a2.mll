{
    type token =
    | INT of int                        (* DEFINING BASIC TOKEN TYPE *)
    | ABS | NOT | DEF
    | PLUS | MINUS | MUL | DIV | MOD | EXP
    | LP | RP
    | TRUE | FALSE
    | AND | OR
    | EQ | GTA | LTA | GEQ | LEQ
    | IF | THEN | ELSE
    | ID of string
    | DELIMITER ;;

    exception InvalidToken of char ;;
}

(* PREDEFINED REGEX *)

(* WHITE SPACE *)
let sp = [' ' '\t' '\n']+

(* INTEGERS *)
let digit = ['0'-'9']
let integer = ('-'|'+')?(['1'-'9']digit* | '0')

(* ID STRING *)
let id = ['a' - 'z'](['a'-'z' 'A'-'Z' '0'-'9'])*


(* READ FUNCTION PARSER *)

rule read = parse
       | sp {read lexbuf}

       | 'T' {TRUE :: (read lexbuf)} | 'F' {FALSE :: (read lexbuf)  (* TRUE AND FALSE BOOLEAN *)}

       | integer as i {match i.[0] with '+' -> (INT(int_of_string (String.sub i 1 (String.length i - 1))) :: (read lexbuf))| _ -> (INT(int_of_string i) :: (read lexbuf)) (* INTEGER TYPE *)}

       | '(' {LP :: (read lexbuf)} | ')' {RP :: (read lexbuf) (* PARENTHESIS *)}

       | "not" {NOT :: (read lexbuf)}
       | "abs" {ABS :: (read lexbuf)        (* SINGLE WORK KEYWORDS *)}
       | "def" {DEF :: (read lexbuf)}

       | "\\/" {OR :: (read lexbuf)} | "/\\" {AND :: (read lexbuf)  (* AND-OR BINARY BOOLEAN OPERATIONS *)}

       | ';' {DELIMITER :: (read lexbuf)  (* END OF COMMAND *)}

       | '+' {PLUS :: (read lexbuf)}
       | '-' {MINUS :: (read lexbuf)}
       | '*' {MUL :: (read lexbuf)                  (* ARITHMETIC OPERATIONS *)}
       | "div" {DIV :: (read lexbuf)}
       | "mod" {MOD :: (read lexbuf)}
       | '^' {EXP :: (read lexbuf)}

       | "if" {IF :: (read lexbuf)}
       | "else" {ELSE :: (read lexbuf)              (* IF THEN ELSE STATEMENTS *)}
       | "then" {THEN :: (read lexbuf)}

       | "=" {EQ :: (read lexbuf)}
       | ">" {GTA :: (read lexbuf)}
       | "<" {LTA :: (read lexbuf)                  (* COMPARE STATEMENTS *)}
       | ">=" {GEQ :: (read lexbuf)}
       | "<=" {LEQ :: (read lexbuf)}

       | id as d {ID(d) :: (read lexbuf)  (* ID STRINGS *)}

       | eof {[]        (* END OF BUFFER *)}

       | _ as w {raise (InvalidToken(w))        (* CHARACTERS THAT ARE NOT IN THE LANGUAGE *)}

(*
---------------------------------------------------------------
       COMMENTED CODE
       HAD BETTER ABSTRACTION OF LITERALS, BUT WAS CREATING TOO MANY STATES (69)
       THE PRESENT STATES NUMBER UPTO (49)
       ONE PRO OF THIS STATE WAS THE SIZE, WHICH WAS AROUND 500 BYTES LESSER THAN THE PRESENT ONE
---------------------------------------------------------------

let sp = [' ' '\t']+        (* SPACE *)

let digit = ['0'-'9']               (* INTEGERS *)
let integer = ('-'|'+')?(['1'-'9']digit* | '0')

let cond = ("if"|"else"|"then")         (* IF THEN ELSE *)

let eq = ['=']
let gl = ['>' '<']                 (* COMPARISON *)
let cmp = (eq|(gl(eq?)))

let binc = ['+' '-' '*' '^']
let bino = ("div"|"mod")          (* BINARY ARITHMETIC *)
let binops = (binc|bino)

let small = ['a'-'z']
let caps = ['A'-'Z']           (* ID STRING *)
let id = small (small|caps|digit)*


(* READ FUNCTION PARSER *)

rule read = parse
      | sp {read lexbuf}

      | 'T' {TRUE :: (read lexbuf)} | 'F' {FALSE :: (read lexbuf)}     (* TRUE AND FALSE BOOLEAN *)

      | integer as i {INT(int_of_string i) :: (read lexbuf)}           (* INTEGER TYPE *)

      | '(' {LP :: (read lexbuf)} | ')' {RP :: (read lexbuf)}     (* PARENTHESIS *)

      | binops as b {(read_binops (Lexing.from_string b)) :: (read lexbuf)}    (* BINARY ARITHMETIC OPERATIONS *)

      | "not" {NOT :: (read lexbuf)}
      | "abs" {ABS :: (read lexbuf)}           (* SINGLE WORK KEYWORDS *)
      | "def" {DEF :: (read lexbuf)}

      | "\\/" {OR :: (read lexbuf)} | "/\\" {AND :: (read lexbuf)}     (* AND-OR BINARY BOOLEAN OPERATIONS *)

      | ';' {DELIMITER :: (read lexbuf)}              (* END OF COMMAND *)

      | cond as c {read_cond(Lexing.from_string c) :: (read lexbuf)}           (* IF THEN ELSE STATEMENTS *)

      | cmp as cp {read_cmp(Lexing.from_string cp) :: (read lexbuf)}           (* COMPARISON OPERATIONS *)

      | id as d {ID(d) :: (read lexbuf)}               (* ID STRINGS *)

      | eof {[]}                               (* END OF BUFFER *)

      | _ as w {raise (InvalidToken(w))}         (* CHARACTERS THAT ARE NOT IN THE LANGUAGE *)

and read_binops = parse
               | '+' {PLUS}
               | '-' {MINUS}             (* SIDE FUNCTION FOR CLASSIFYING BINARY ARITHMETIC OPERATIONS *)
               | '*' {MUL}
               | "div" {DIV}
               | "mod" {MOD}
               | '^' {EXP}

and read_cond = parse
               | "if" {IF}
               | "else" {ELSE}        (* SIDE FUNCTION FOR CLASSIFYING CONDITIONALS *)
               | "then" {THEN}

and read_cmp = parse
               | "=" {EQ}
               | ">" {GTA}
               | "<" {LTA}             (* SIDE FUNCTION FOR CLASSIFYING COMPARISON OPERATIONS *)
               | ">=" {GEQ}
               | "<=" {LEQ}
*)


{
    let scanner s = read (Lexing.from_string s)         (* LEXER FUNCTION *)
}


(*
    EXAMPLES AND COUNTER EXAMPLES

EXAMPLES
    1. scanner " < T  abs not+ <  F   not a4Ba1cD* T   T b<= d  T cD dcD mod ^>= dcDa1cDa1b+-cD then)  if ; ba1d a4B  not not a1cDcDa1b then )  if  abs>=cDa1b"
    token list =
    [LTA; TRUE; ABS; NOT; PLUS; LTA; FALSE; NOT; ID "a4Ba1cD"; MUL; TRUE; TRUE;
     ID "b"; LEQ; ID "d"; TRUE; ID "cD"; ID "dcD"; MOD; EXP; GEQ; ID "dcDa1cDa1b";
     PLUS; MINUS; ID "cD"; THEN; RP; IF; DELIMITER; ID "ba1d"; ID "a4B"; NOT; NOT;
     ID "a1cDcDa1b"; THEN; RP; IF; ABS; GEQ; ID "cDa1b"]

    2. scanner " a1cDcDa1b -b mod mod ifcD a4B else cD) )def bc  F   else bdiv   if  T )- <  T  not a4Ba1cD+a1b cD abs <"
    token list =
    [ID "a1cDcDa1b"; MINUS; ID "b"; MOD; MOD; ID "ifcD"; ID "a4B"; ELSE;
     ID "cD"; RP; RP; DEF; ID "bc"; FALSE; ELSE; ID "bdiv"; IF; TRUE; RP; MINUS;
     LTA; TRUE; NOT; ID "a4Ba1cD"; PLUS; ID "a1b"; ID "cD"; ABS; LTA]

    3. scanner " d cD  else def dcD abs + ; a1a1cDa1bcD  not def  not - *a1* (dcDa1b then  abs ) notdcD ) bcDc cDa1cDa1b  else  abs a4Ba1cD F cDda1<= mod  else bc a4B^da1a4B < a4Bd  mod bcDc a1dba1d F def  ^= T  T ; <= def c  elsecDa1cDa1b then mod notcDa1b  absa4Ba1cDcD> a1d= not  then d  F  -  if bcDca1cDcDa1b = a1cDcDa1b dcDb moda4BcD  if  if ifba1d abs ) > a1 cD  F div  F * not else ba1da1cDcDa1b  mod= bcDc if  T div ) then F  abs b ba1 * (  absd"
    token list =
    [ID "d"; ID "cD"; ELSE; DEF; ID "dcD"; ABS; PLUS; DELIMITER; ID "a1a1cDa1bcD";
     NOT; DEF; NOT; MINUS; MUL; ID "a1"; MUL; LP; ID "dcDa1b"; THEN; ABS; RP;
     ID "notdcD"; RP; ID "bcDc"; ID "cDa1cDa1b"; ELSE; ABS; ID "a4Ba1cD"; FALSE;
     ID "cDda1"; LEQ; MOD; ELSE; ID "bc"; ID "a4B"; EXP; ID "da1a4B"; LTA;
     ID "a4Bd"; MOD; ID "bcDc"; ID "a1dba1d"; FALSE; DEF; EXP; EQ; TRUE; TRUE;
     DELIMITER; LEQ; DEF; ID "c"; ID "elsecDa1cDa1b"; THEN; MOD; ID "notcDa1b";
     ID "absa4Ba1cDcD"; GTA; ID "a1d"; EQ; NOT; THEN; ID "d"; FALSE; MINUS; IF;
     ID "bcDca1cDcDa1b"; EQ; ID "a1cDcDa1b"; ID "dcDb"; ID "moda4BcD"; IF; IF;
     ID "ifba1d"; ABS; RP; GTA; ID "a1"; ID "cD"; FALSE; DIV; FALSE; MUL; NOT;
     ELSE; ID "ba1da1cDcDa1b"; MOD; EQ; ID "bcDc"; IF; TRUE; DIV; RP; THEN; FALSE;
     ABS; ID "b"; ID "ba1"; MUL; LP; ID "absd"]

     4. scanner "043"
     token list = [INT 0; INT 43]

     5. scanner "This File"
     token list = [TRUE; ID "his"; FALSE; ID "ile"]

COUNTER EXAMPLES
    1. scanner "ill E (div   F !@ if!   <=CrBaAasBaCra1 < b, not     abs=, <=  AasZshAasAasAasBaCr   ;   F EBa   a4BAas ba1d";;
    Exception: InvalidToken 'E'.

    2. scanner "ill cD<AasBaCrEEE < not * def  cDa1cDa1b not  @  ! div     , bc$ = def <= then =a1cDcDa1b def  div   ( abs^  CrBaAasBaCr    T >< >  not  BabAasa1bEAasZshdef, F   AasBaAas abs = a1bCrBaAasBaCr not   not a1b"
    Exception: InvalidToken 'A'.

    3. scanner "ill   ,ZshAasAas *AasBaCr   a1cDcDa1b a1  ZshBaCrAas if thena1cDa1bcDdef >(AasCrZshcDEBa div   mod   if (     >< F def ^          not a4Ba1cD  ZshAasAas div AasAasAas  Aas  T    <=a1b    mod"
    Exception: InvalidToken ','.

    4. scanner "cop290 #";;
    Exception: InvalidToken '#'.
 *)
