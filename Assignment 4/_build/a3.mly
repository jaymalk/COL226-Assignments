%{
    open A1
    exception Bad_State
%}

/* DEFINING TOKENS. To be used in lexer */
/* EXPRESSIONS */
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
/* For function specifications */
%token BACKSLASH DOT COLON ARROW
/* Boolean Constants */
%token  <bool> BOOL
/* Integer constants */
%token  <int> INT
/* String ids */
%token  <string> ID

/* TYPES */
/* All possible type-builders for new assignment */
%token TT, TB, TP, TF

/* DEFINITIONS */
/* Definitional Keyword */
%token DEF
/* Local Definitions (BOTH EXPRESSIONS AND DEFINITIONS) */
%token LET IN END LOCAL
/* Possible recursive structure */
%token SEMICOLON PARALLEL

/* Command Delimiter */
%token DELIMITER
/* End of line marker */
%token  EOF



/* STARTING GRAMMAR */
%start def_parser exp_parser type_parser
%type <A1.definition> def_parser /* Returns definitions */
%type <A1.exptree> exp_parser /* Returns expression */
%type <A1.exptype> type_parser /* Returns types */
%%

/* EXPRESSION GRAMMAR */
exp_parser:
    /* Main */
    | premain EOF                   { $1 }

premain:
    /* Boolean | Conditional | Arithmetic expressions */
    | expression                  { $1 }
    /* Tuple related */
    /* | tuple                       { $1 } */
    /* | projections                 { $1 } */

/* CONDITIONAL LAYER */
/* conditional: */
    /* IF premain THEN premain ELSE premain FI    {IfThenElse($2, $4, $6)} */

/* PROJECTIONS */

  /* | PROJ LP INT COMMA INT RP ID    {Project(($3, $5), Var($7))} */

/* MAIN EXPRESSIONS (BOOL AND ARITHMETIC) */
expression:
    or_expression                             { $1 }

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
    NOT not_expression                        {Not($2)}
    | arithmetic_expression                   { $1 }
    /* Comparison */
    | comparison                              { $1 }

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
    mult_expression TIMES div_expression      {Mult($1, $3)}
    | div_expression                          { $1 }

div_expression:
    div_expression DIV rem_expression         {Div($1, $3)}
    | rem_expression                          { $1 }

rem_expression:
    rem_expression REM unary_arithmetic       {Rem($1, $3)}
    | unary_arithmetic                        { $1 }

unary_arithmetic:
    projections                               { $1 }
    | ABS unary_arithmetic                    {Abs($2)}
    | TILDA unary_arithmetic                  {Negative($2)}

/* Projections */
projections:
    PROJ LP INT COMMA INT RP projections      {Project(($3, $5), $7)}
    | func_call                               { $1 }


/* FUNCTIONS */
func_call:
    /* Call from projection */
    | func_call func_abs            {FunctionCall($1, $2)}
    | func_abs                      { $1 }

func_abs:
    BACKSLASH ID COLON type_parser DOT func_abs      {FunctionAbstractionType($2, $4, $6)}
    | conditional                                    { $1 }

/* Conditional */
conditional:
    | IF premain THEN premain ELSE premain FI    {IfThenElse($2, $4, $6)}
    | tuple                                { $1 }

/* TUPLE EXPRESSIONS */
tuple:
    LP RP                         {Tuple(0, [])}
    | LP tuple_list RP            { $2 }
    | basic_unit                  { $1 }

tuple_list:
    premain COMMA premain               {Tuple(2, [$1; $3])}
    | premain COMMA tuple_list          {match $3 with Tuple(x, el) -> Tuple(x+1, $1::el) | _ -> raise Bad_State}


basic_unit:
    /* For integers */
    INT                                       {N($1)}
    /* For bools */
    | BOOL                                    {B($1)}
    /* For identity markers */
    | ID                                      {Var($1)}
    /* For bracketed expressions */
    | LP premain RP                           {InParen($2)}
    /* Local definitions in */
    | LET predef IN premain END               {Let($2, $4)}

/* DEFINITION GRAMMAR */
def_parser:
    predef  EOF                            {$1}

predef:
    /* Local definitions */
    LOCAL predef IN predef END     {Local($2, $4)}
    | series                       { $1 }

series:
    /* Parallel & Sequential definitions */
    | series SEMICOLON basic_def               {Sequence([$1; $3])}
    | series PARALLEL PARALLEL basic_def       {Parallel([$1; $4])}
    | basic_def                                { $1 }

basic_def:
    /* Simplest definition */
    | DEF ID COLON type_parser EQ premain      {SimpleType($2, $4, $6)}
;

/* TYPE GRAMMAR */
type_parser:
    | type_parser ARROW tuple_type          {Tfunc($1, $3)}
    | tuple_type                            { $1 }

tuple_type:
    | basic_type TIMES tuple_type           {match $3 with Ttuple(x) -> Ttuple($1::x) | x -> Ttuple($1::x::[])}
    | basic_type                            {$1}

basic_type:
    | TT                                    {Tint}
    | TB                                    {Tbool}
    | LP type_parser RP                     {$2}
