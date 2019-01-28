# Definitional Interpreter and Stack Machine

### Syntax
**The abstract syntax of calculator is characterised by the type**

``` ocaml
type  exptree = N of bigint 
          |  Plus of exptree *  exptree 
          | Minus of exptree *  exptree 
          |  Mult of exptree *  exptree 
          |   Div of exptree *  exptree 
          |   Rem of exptree *  exptree 
          |   Neg of exptree 
          |   Abs of exptree 
```

### Interpreter

**The definitional interpreter is to be defined as the following function**
``` ocaml
  val eval : exptree -> bigint
```

### Stack Machine and Compiler

**The type of opcodes of the stack machine is defined as**
``` ocaml
type opcode = CONST of bigint | PLUS | TIMES | MINUS | DIV | REM | ABS | UNARYMINUS
```

**The stack machine is to be defined as a tail-recursive function**
```ocaml
  stackmc: (bigint list) -> (opcode list) -> bigint
```

_The compile function is simply a postorder traversal of an abstract syntax tree of an expression_ <br>
**The compiler is to be defined as a recursive function**
``` ocaml
  compile: exptree -> (opcode list)
```
****
### Proof
Also, included is the [proof](./proof.pdf) of the theorem <br>
**Theorem** : **compiler with stackmc** *calculates a bigint whose value is the same as what* **eval** *computes.*
