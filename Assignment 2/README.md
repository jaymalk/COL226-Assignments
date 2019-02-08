# Building a scanner using OCaml-Lex

### Specify tokens for a simple arithmetic and boolean calculation language and then generate a scanner for defined tokens


#### The expressions in the language are of the following forms...
- _Integer constants_ which have an optional sign followed by at least one digit without useless leading zeroes.
- _Unary arithmetic operations_:
    - **abs**
- _Binary operations_:
    - **+**  `addition`
    - **-**  `subtraction`
    - **\***  `multiplication`
    - **div**
    - **mod**
    - **^**  `exponentiation`
- _Parentheses_:
    - **(**
    - **)**
- _Boolean constants_:
    - **T** and
    - **F**
- _Unary boolean operation_:
    - **not**
- _Binary boolean operations_:  
    - **/\**  `and`
    - **\/**  `or`
- _Comparison operators_:
    - **=**  `equal`
    - **>**  `greater than`
    - **<**  `less than`
    - **>=**  `greater or equal`
    - **<=**  `less or equal`
- _A conditional operator_ consisting of three tokens:
    - **if then else**
- _Identifiers_ which are alphanumeric strings beginning with lower-case letter.
- _A definition construct_:
    - **def**
- _A delimiter_ to terminate the expression:
    - **;**
