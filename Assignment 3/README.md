# Parsing for a simple expression evaluator

## Design a grammar for a simple expression language, taking care to enforce precedence rules (e.g., BODMAS).

### Generate an abstract syntax tree from the parsed data. Then, use it for further evaluation.

Expressions include...
  - Variables _(starting with a Capital letter, represented as alphanumeric strings with_ (\_) _and (') )_
  - Integer constants
  - Boolean constants
  - Expressions using unary operators on integers...
      - Unary Minus `~`
      - Absolute Value `abs`
  - Expressions using unary operators on booleans...
      - Negation `not`
  - Expressions using binary operators on integers...
      - Addition `+`
      - Subtraction `-`
      - Multiplication `*`
      - Division and Modulo `div | mod`
  - Expressions using binary operators on booleans...
      - Conjunction `\/`
      - Disjunction `/\`
  - Expressions using comparison operations on integers
      - `=`
      - `>=`
      - `<=`
      - `>`
      - `<`
  - Expressions using parenthesis...
      - `(`
      - `)`
  - Conditional Expression `if __ then __ else ___ fi`
  - Expressions for creating _N-Tuples_
  - Expressions for projecting the `i-th` element of an expression (which evaluates to an _N-Tuple_, and `1 <= i <= n`)

