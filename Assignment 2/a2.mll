{
    type token =
    | INT of int
    | ABS | NOT | DEF
    | ADD | MINUS | MUL | DIV | MOD | EXP
    | LP | RP
    | TRUE | FALSE
    | AND | OR
    | EQ | GTA | LTA | GEQ | LEQ
    | IF | THEN | ELSE
    | ID of string
    | DELIMITER ;;

    exception UNRECOGNISED of char ;;
    exception NOT_ALLOWED of string ;;
}

(* PREDEFINED REGEX *)

let sp = [' ' '\t']+        (* SPACE *)

let digit = ['0'-'9']               (* INTEGERS *)
let integer = ('-'|'+')?['1'-'9']digit*

let cond = ("if"|"else"|"then")         (* IF THEN ELSE *)

let eq = ['=']
let gl = ['>' '<']                 (* COMPARISON *)
let cmp = (eq|(gl(eq?)))

let binc = ['+' '-' '*' '^']
let bino = ("div"|"mod")          (* BINARY ARITHMETIC *)
let binops = (binc|bino)

let small = ['a'-'z']
let caps = ['A'-'Z']           (* ID STRING *)
let id = small (small|caps)*

let caps_words = (caps)(small|caps)*


(* READ FUNCTION PARSER *)

rule read = parse
       | sp {read lexbuf}

       | caps_words as l {raise (NOT_ALLOWED(l))}

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

       | _ as w {raise (UNRECOGNISED(w))}          (* CHARACTERS THAT ARE NOT IN THE LANGUAGE *)

and read_binops = parse
                | '+' {ADD}
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


{
    let scanner s = read (Lexing.from_string s)         (* LEXER FUNCTION *)
}
