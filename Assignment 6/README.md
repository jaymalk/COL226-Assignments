# A simulator for nested procedure calls
In this assignment I have implemented the core ideas for understanding implementations of the (static/lexical) scoping discipline in **Algol-like languages**, *particularly the visibility rules and the set-up/tear-down involved in procedure call and return*.

 - It is a general **REPL** and allows following command-specific executions.
   - **Variable Related**
     - Set the value of a variable to a given value/expression. `x := y;` or `x := 3;`. 
     - The correct instance of variable is updated.  
     - If a varaible is not accessible, an informative error message is displayed. 
   - **Procedure Related**
     - Call a procedure with specified arguments. `call P(c,7);` or  `call S(a,5);`
     - If the procedure call is legal, then *the called procedure's frame with the parameters bound to the actual arguments* is stacked up and the static links/display registers, and the dynamic links are correctly updated.  
     - If the call is not legal (procedure is not callable, or the arguments do not match the number of parameters), then an informative error message is displayed.
   - **Return**
     - Return from the current procedure to caller. 
     - The current call stack is popped, and the static links/display registers are correctly updated.
     
 - It is also made sure that at any point in the simulation, following is visible
   - **The call-stack** 
     - `trace;`
   - What procedures can be called from the current procedure frame at the top of the stack. 
     - `call;`
   - All the *named variables* accessible from the current procedure frame, and their current values.
   - The **static link chain**.
   
****
Reference : **Programming Language Pragmatics** by *Michael L. Scott* -> *Chapter : Subroutines and Control Abstraction*
