# CBV and CBN Interpreters

### For the language, I have designed and implemented (in OCaml) the following abstract machines...

*Note : Both of the machines first compile the program in their desired format and then operate on the compiled code accordingly*

 - **The Krivine Machine** *(in closure form)*, that implements **Call-by-Name** semantics.
  The structure of `krivine` goes as...
    
```ocaml
  val krivine_compile : abstract_tree -> compiled list
  val krivine : stack -> environment -> compiled list -> answer_closure
 ```
 
 - **The SECD Machine** that implements **Call-by-Value** semantics.
  The structure of `secd` goes as...
   
```ocaml
  val secd_compile : abstract_tree -> compiled list
  val secd : stack -> environment -> compiled list -> dump -> answer
 ```

### Features
 - **Interpreter** is implemented for both the languages. Environment variable can be natively worked with using definitions.
 - In case of `secd` program stored in a file can be compiled and run to give the answer.
 - *Recursion* works and we are very well able to run factorial or fibonacci functions! **Tail recursion** is also optimised, due to tail-recursive optimisation in ocaml.

