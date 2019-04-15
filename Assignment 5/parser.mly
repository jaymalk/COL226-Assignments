%{
    open Exptree
    open Bigint
    exception Bad_State
%}

/* DEFINING TOKENS. To be used in lexer */
/* EXPRESSIONS */
/* Left and right parenthesis */
%token  /* Simple */ LP RP /* Square */ LS RS
/* Seperator and projector for n-tuples */
%token  COMMA PROJ SLASH
/* Conditional keywords */
%token  IF THEN ELSE FI
/* Arithmetic operations */
%token  TILDA ABS PLUS MINUS TIMES DIV REM
/* Boolean operations */
%token  NOT CONJ DISJ
/* Comparison operations */
%token  EQ GT LT CMP
/* For function specifications */
%token BACKSLASH DOT HASH FUNC ARROW REC
/* For while loops */
%token WHILE LC RC
/* Boolean Constants */
%token  <bool> BOOL
/* Big Integer constants */
%token <int> INT
/* String ids */
%token  <string> ID

/* DEFINITIONS */
/* Definitional Keyword */
%token DEF
/* Local Definitions (BOTH EXPRESSIONS AND DEFINITIONS) */
%token LET IN END LOCAL
/* Possible recursive structure */
%token SERIES PARALLEL

/*  Handler */
%token EOC WATCH EXIT
/* End of line marker */
%token  EOF



/* STARTING GRAMMAR */
%start def_parser exp_parser
%type <Exptree.definition> def_parser /* Returns definitions */
%type <Exptree.exptree> exp_parser /* Returns expression */
%%

/* EXPRESSION GRAMMAR */
exp_parser:
    /* Main */
    | premain EOC                   { $1 }
    | EOC                           { Null }
    | EOF                           { Null }
    | WATCH                         { Watch }
    | EXIT                          { Exit }

premain:
    /* Expressions */
    | expression                              { $1 }

/* MAIN EXPRESSIONS (BOOL AND ARITHMETIC) */
expression:
    or_expression                             { $1 }

/* COMPARISON */
comparison:
    CMP arithmetic_expression                             {Cmp($2)}  
    | arithmetic_expression EQ arithmetic_expression      {Equals($1, $3)}
    | arithmetic_expression GT arithmetic_expression      {GreaterT($1, $3)}
    | arithmetic_expression LT arithmetic_expression      {LessT($1, $3)}
    | arithmetic_expression GT EQ arithmetic_expression   {GreaterTE($1, $4)}
    | arithmetic_expression LT EQ arithmetic_expression   {LessTE($1, $4)}

/* Boolean layer taking arithmetic as a basic unit */
or_expression:
    and_expression DISJ or_expression         {Or($1, $3)}
    | and_expression                          { $1 }

and_expression:
    not_expression CONJ and_expression        {And($1, $3)}
    | not_expression                          { $1 }

not_expression:
    NOT not_expression                        {Not($2)}
    /* Comparison */
    | comparison                              { $1 }
    | arithmetic_expression                   { $1 }

/* Arithmetic expressions (and operations) end here */
arithmetic_expression:
    add_expression                            { $1 }

add_expression:
    add_expression PLUS sub_expression        {Plus($1, $3)}
    | sub_expression                          { $1 }

sub_expression:
    sub_expression MINUS mult_expression      {Minus($1, $3)}
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
    | conditional LS INT SLASH INT RS         {Project(($3, $5), $1)}
    | conditional                             { $1 }

/* Conditional */
conditional:
    | IF premain THEN premain ELSE premain FI    {If_Then_Else($2, $4, $6)}
    | func_call                                  { $1 }

/* FUNCTIONS */
func_call:
    /* Call from projection */
    | func_call func_abs            {App($1, $2)}
    | func_abs                      { $1 }

func_abs:
    BACKSLASH ID DOT func_abs      {Lambda($2, $4)}
    | FUNC ID ARROW func_abs       {Lambda($2, $4)}
    | tuple                        { $1 }

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
    INT                                       {Integer($1)}
    /* For bools */
    | BOOL                                    {Bool($1)}
    /* For identity markers */
    | ID                                      {V($1)}
    /* For bracketed expressions */
    | LP premain RP                           {InParen($2)}
    /* Local definitions in */
    | LET predef IN premain END               {Let($2, $4)}
    /* Definition Reading Required */
    | HASH                                    {raise Exptree.Read_Definition}

/* DEFINITION GRAMMAR */
def_parser:
    predef EOC                                 { $1 }

predef:
    /* Local definitions */
    LOCAL predef IN predef END                 {Local($2, $4)}
    | series                                   { $1 }

series:
    /* Sequential definitions */
    | series_build                             { $1 }
    | parallel                                 { $1 }

series_build:
    | parallel SERIES parallel               {Sequence([$1; $3])}
    | parallel SERIES series_build           {match $3 with Sequence(dl) -> Sequence($1::dl) | _ -> raise Bad_State}

parallel:
    /*  Parallel definitions */
    | par_build                                { $1 }
    | while_loop                               { $1 }

par_build:
    | while_loop PARALLEL PARALLEL while_loop    {Parallel([$1; $4])}
    | while_loop PARALLEL PARALLEL par_build     {match $4 with Parallel(dl) -> Parallel($1::dl) | _ -> raise Bad_State}

while_loop:
    WHILE premain LC predef RC                 {While($2, $4)}
    | basic_def                                { $1 }


basic_def:
    /* Simplest definition */
    | DEF ID EQ premain                        {SimpleType($2, $4)}
    | DEF ID EQ REC FUNC ID ARROW func_abs     {SimpleType($2, RecFuncAbs($2, $6, $8))}
    | LP predef RP                             { $2 }
;
