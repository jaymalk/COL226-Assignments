(*
    LEXER
    - For lexing the linear string from stdin
    - Uses tokens defined and returns them back to the parser
*)

(* Main file declarations *)
{
  open Parser
  exception Bad_Char of char
}

let caps = ['A'-'Z']+
let small = ['a'-'z']+

let integer = ['0'-'9']+

let body = [' ' '\t' '\n' 'A'-'Z' 'a'-'z']*

let sp = [' ' '\t' '\n']+

(* Rule for parsing the tokens *)
rule read = parse
    sp                  { read lexbuf  (* Ignoring white space. *)}

|   '('                 { LP        (* Left Parathesis *)}
|   ')'                 { RP        (* Right Parathesis *)}

|   ('{')(body)('}')    { BODY      (* Body of a procedure *)}

|   "program"           { PROCEDURE       (* Program Key Word *)}
|   "procedure"         { PROCEDURE       (* Procedure Key Word *)}
|   "var"               { VAR             (* Var Key Word *)}
|   "call"              { CALL            (* Calling keyword *)}
|   "exit"              { exit(0) }
|   "Main"              { MAIN }
|   "trace"             { TRACE }
|   "return"            { RETURN }

|   integer as n        { INT(int_of_string n) }

|   ':'                 { COLON }
|   ';'                 { SEMICOLON }
|   ','                 { COMMA }
|   '='                 { EQ }

|   '?'                 { WATCH (* For viewing table contents *)}
|   "exit"              { EXIT (* Exiting the interpreter *)}

(* Capturing Types *)
|   "Tint"              { TYPE(Tint) }

|   small as s          { SMALL(s) (* Small case characters *)}
|   caps  as c          { CAPS(c)  (* Capital case characters *)}

|   eof                 { EOF          (* End of file marker *)}

|   _ as s              { raise (Bad_Char(s)) }
