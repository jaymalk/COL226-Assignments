In this assignment, we have specified the tokens for a simple arithmetic and boolean calculation language.
Then, we have generated a scanner for these tokens.  

The expressions in the language are of the following forms
- Integer constants, *which have an optional sign, followed by at least one digit, without useless leading zeroes*.
- Unary arithmetic operations: **abs**
- Binary operations: **+** (`addition`), **-** (`subtraction`),  * (`multiplication`), **div**, **mod**, **^** (`exponentiation`)
- Parentheses: **(** , **)**
- Boolean constants: **T** and **F**
- Unary boolean operation: **not**
- Binary boolean operations:  **/\** (`and`), **\/** (`or`)
- Comparison operators: **=** (`equal`) , **>** (`greater than`), **<** (`less than`) , **>=** (`greater or equal`), **<=** (`less or equal`)
- A conditional operator consisting of three tokens: **if then else**
- Identifiers, which are alphanumeric strings beginning with lower-case letter.
- A definition construct: **def**
- A delimiter to terminate the expression: **;**
