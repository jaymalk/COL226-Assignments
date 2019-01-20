# Big Integer Package
### OCaml

*Creating a package, using* **OCaml** *programming language, to facilitate various mathematical operations.*

- *The basic declaration is*
```OCaml
type sign = Neg | NonNeg;;
type bigint = sign*(int list);;
```
<sub>_with the representational invariant that the elements of the int list are between 0 and 9, and are presented most significant digit first, and that there are no unnecessary leading zeros._

*Following operations are also to be implemented*
- Arithmetic operations:

    - Addition.  
```Ocaml
    add: bigint -> bigint -> bigint
```
   - Multiplication.  
```Ocaml
    mult: bigint -> bigint -> bigint
```
   - Subtraction.  
```Ocaml
    sub: bigint -> bigint -> bigint
```
   - Quotient:   
```Ocaml
    div: bigint -> bigint -> bigint
```
   - Remainder.  
```Ocaml
    rem: bigint -> bigint -> bigint
```
   - Unary negation.  
```Ocaml
    minus: bigint -> bigint
```
   - Absolute value.  
```Ocaml
    abs: bigint -> bigint
```

- Comparison operations:

    - Equal.   
    ```OCaml
    eq: bigint -> bigint -> bool
    ```

    - Greater_than.  
    ```OCaml
    gt:  bigint -> bigint -> bool
    ```

    - Less_than.  
    ```OCaml
    lt:  bigint -> bigint -> bool
    ```

    - Great_or_equal.  
    ```OCaml
    geq:  bigint -> bigint -> bool
    ```

    - Less_or_equal.  
    ```OCaml
    leq:  bigint -> bigint -> bool
    ```

- Functions to present the result in the form of a string
    ``` Ocaml
    print_num:  bigint -> string
    ```
- Conversion functions from OCaml int to bigint.
    ```ocaml
    mk_big:  int -> bigint
    ```
