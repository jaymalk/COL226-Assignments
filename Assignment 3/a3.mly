/*
    PARSER
    - For making sense of info passed by lexer using a defined grammar
    - Based on a basic CFG
    - Returns the abstract syntax tree by parsing
*/

%{
    open A1
%}

/* DEFINING TOKENS. To be used in lexer */
/* Left and right parenthesis */
%token  LP RP
/* Seperator and projector for n-tuples */
%token  COMMA PROJ
/* Conditional keywords */
%token  IF THEN ELSE FI
/* Arithmetic operations */
%token  TILDA ABS PLUS MINUS TIMES DIV REM
/* Boolean operations */
%token  NOT CONJ DISJ
/* Comparison operations */
%token  EQ GTA LTA GEQ LEQ
/* Boolean Constants */
%token  <bool> BOOL
/* Integer constants */
%token  <int> INT
/* String ids */
%token  <string> ID
/* Definitional Keyword */
%token DEF
/* Command Delimiter */
%token DELIMITER
/* End of line marker */
%token  EOF

/* STARTING GRAMMAR */
/* Defining the start symbol for the grammar */
%start main
/* Referring to the return type as 'abstract syntax tree' */
%type <A1.exptree> main
/* start */
%%

main:
    EOF     {Done}
