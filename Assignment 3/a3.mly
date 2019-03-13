/*
    PARSER
    - For making sense of info passed by lexer using a defined grammar
    - Based on a basic CFG
    - Returns the abstract syntax tree by parsing
*/

%{
    open A1
    exception Bad_State
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
%token  EQ GT LT
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
/* PRECEDENCE ORDER */
/* InParen > Tuple > Project > IfThenElse > Negative > Abs > Div > Mult > Rem > Add > Sub > (GreaterT = LessT = GreaterTE = LessTE = Equals) > Not > Conjunction > Disjunction */

main:
      /* Main */
      | premain EOF                   { $1 }

premain:
    /* Boolean | Conditional | Arithmetic expressions */
    | expression                  { $1 }
    /* Tuple related */
    | tuple                     { $1 }
    | projections               { $1 }

/* CONDITIONAL LAYER */
conditional:
    IF expression THEN premain ELSE premain FI    {IfThenElse($2, $4, $6)}


/* TUPLE EXPRESSIONS */
tuple:
    LP RP                         {Tuple(0, [])}
    | LP tuple_list RP            { $2 }

tuple_list:
    premain COMMA premain               {Tuple(2, [$1; $3])}
    | premain COMMA tuple_list       {match $3 with Tuple(x, el) -> Tuple(x+1, $1::el) | _ -> raise Bad_State}

/* PROJECTIONS */
projections:
    PROJ LP INT COMMA INT RP premain {Project(($3, $5), $7)}

/* MAIN EXPRESSIONS (BOOL AND ARITHMETIC) */
expression:
    or_expression                             { $1 }
    | comparison                              { $1 }

/* COMPARISON */
comparison:
    arithmetic_expression EQ arithmetic_expression        {Equals($1, $3)}
    | arithmetic_expression GT arithmetic_expression      {GreaterT($1, $3)}
    | arithmetic_expression LT arithmetic_expression      {LessT($1, $3)}
    | arithmetic_expression GT EQ arithmetic_expression   {GreaterTE($1, $4)}
    | arithmetic_expression LT EQ arithmetic_expression   {LessTE($1, $4)}

/* Boolean layer taking arithmetic as a basic unit */
or_expression:
    and_expression DISJ or_expression         {Disjunction($1, $3)}
    | and_expression                          { $1 }

and_expression:
    not_expression CONJ and_expression        {Conjunction($1, $3)}
    | not_expression                          { $1 }

not_expression:
    NOT arithmetic_expression                 {Not($2)}
    | arithmetic_expression                   { $1 }

/* Arithmetic expressions (and operations) end here */
arithmetic_expression:
    add_expression                            { $1 }

add_expression:
    add_expression PLUS sub_expression        {Add($1, $3)}
    | sub_expression                          { $1 }

sub_expression:
    sub_expression MINUS mult_expression      {Sub($1, $3)}
    | mult_expression                         { $1 }

mult_expression:
    div_expression TIMES mult_expression      {Mult($1, $3)}
    | div_expression                          { $1 }

div_expression:
    rem_expression DIV div_expression         {Div($1, $3)}
    | rem_expression                          { $1 }

rem_expression:
    unary_arithmetic REM rem_expression       {Rem($1, $3)}
    | unary_arithmetic                        { $1 }

unary_arithmetic:
    basic_unit                                { $1 }
    | ABS unary_arithmetic                    {Abs($2)}
    | TILDA unary_arithmetic                  {Negative($2)}

basic_unit:
    /* For integers */
    INT                                       {N($1)}
    /* For bools */
    | BOOL                                    {B($1)}
    /* For identity markers */
    | ID                                      {Var($1)}
    /* For bracketed expressions */
    | LP premain RP                           {InParen($2)}
    /* Conditional operations */
    | conditional                             {$1}
