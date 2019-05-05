%{
    open Procedure
%}

/* DEFINING TOKENS. To be used in lexer */
/* Left and right parenthesis */
%token  /* Simple */ LP RP
/* Semicolon and colon */
%token  SEMICOLON COMMA COLON
/* String ids, lower and capital case */
%token  <string> SMALL
%token  <string> CAPS
%token  <int>    INT
/* Supposed body of a procedure, to be skipped */
%token BODY
/* Token for capturing variable type */
%token  <Procedure.types> TYPE
/* Keywords */
%token PROCEDURE, VAR
/* Calling mechanism */
%token CALL
/* --------- */
/*  Handler */
%token WATCH EXIT
/* End of line marker */
%token  EOF



/* STARTING GRAMMAR */
%start procedure_parser dynamic_parser
%type <Procedure.call> dynamic_parser /* Return user calls */
%type <Procedure.procedure> procedure_parser /* Return a procedure */
%%

dynamic_parser:
    | CALL CAPS LP input_list RP                { Call($2, $4)}

input_list:
    | input                                     { [$1] }
    | input_list COMMA input                    { $1 @ [$3] }

input:
    | SMALL                                     { V($1) }
    | INT                                       { N($1) }

procedure_parser:
    | PROCEDURE CAPS SEMICOLON variable_declaration SEMICOLON procedure_list BODY
            { Procedure("Main", [], $4, $6) }
    | PROCEDURE CAPS variable_declaration SEMICOLON variable_declaration SEMICOLON procedure_list BODY
            { Procedure($2, $3, $5, $7) }
    | PROCEDURE CAPS variable_declaration SEMICOLON variable_declaration SEMICOLON BODY
            { Procedure($2, $3, $5, []) }

procedure_list:
    | procedure_parser                            { [$1] }
    | procedure_parser procedure_list             { $1 :: $2 }

variable_declaration:
    | VAR variable_declaration                    { $2 }
    | variable_input                              { $1 }

variable_input:
    | LP variable_input RP                        { $2 }
    | variable_list                               { $1 }

variable_list:
    | variable                                    { [$1] }
    | variable_list COMMA variable                { $1 @ [$3] }

variable:
    SMALL COLON TYPE                              { Var($1, $3) }
