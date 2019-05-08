# Language extension and type checking

The language is now extended to include the following new features.

**Expressions** include:
 - **Variables** :*starting with a capital-letter, represented as alphanumeric strings with underscores \_ and apostrophes \'*
 - *Integer constants*
 - *Boolean constants*
 - *Expressions using unary operators on integers: unary minus (~) , abs*
 - *Expressions using unary operators on booleans: not*
 - *Expressions using binary operators on integers: addition, subtraction, multiplication, div and mod (+, -, \*, div, mod)*
 - *Expressions using binary operators on booleans: conjunction, disjunction (/\\, \\/)*
 - *Expressions using comparison operations on integers (=, >=, <=, >, <)*
 - *Expressions using parenthesis: \(  \)*
 - **Conditional expression** `if __ then __ else ___ fi`
 - *Expressions for creating n-tuples* `n = 0 or n > 1`
 - *Expressions for projecting the i-th component of an expression (which evaluates to an n-tuple, and 1 <= i <= n)*
 - *Expressions using locally-scoped definitons* `let d in e end`
 - *Expressions for function abstractions on one variable and function call* : `\x.e` \& `e1(e2)`
 
 and
 
 **Definitions** are :
 - Simple : `def x = e`
 - Sequential : `d1 ; d2`
 - Parallel : `d1 || d2` 
 - Locally Scoped : `local d1 in d2 end`
 
****
**Type Checking** </br>
 Type is defined as followed. </br>
 ``` ocaml
  Type = Tint | Tunit | Tbool |  t1 * .... * tn | t1 to t2
 ```
Now for a *type-checker* for this language.  
Let **G** be a set of type assumptions on variables.

- The type association `G |- e : t` is provided by
``` ocaml
 val hastype : gamma -> expression -> exptype
```
- Definition yield `G |- d : G[G']` is provided by
``` ocaml
 val yields: gamma -> definition -> gamma
```
