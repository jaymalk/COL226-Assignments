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
/* Supposed body of a procedure, to be skipped */
%token BODY
/* Token for capturing variable type */
%token  <Procedure.types> TYPE
/* Keywords */
%token PROGRAM, PROCEDURE, VAR, MAIN
/* --------- */
/*  Handler */
%token WATCH EXIT
/* End of line marker */
%token  EOF



/* STARTING GRAMMAR */
%start procedure_parser program_parser
%type <Procedure.program> program_parser /* Return complete program */
%type <Procedure.procedure> procedure_parser /* Return a procedure */
%%

/* PROGRAM GRAMMAR */
program_parser:
    | PROGRAM MAIN SEMICOLON variable_declaration SEMICOLON procedure_list BODY
        { Main($4, $6) }
    | PROGRAM MAIN SEMICOLON variable_declaration SEMICOLON BODY
        { Main($4, []) }

procedure_parser:
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
