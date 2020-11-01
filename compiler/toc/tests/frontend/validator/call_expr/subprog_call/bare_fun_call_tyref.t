/*
 - Bare Function call, type ref
   - Should always evaluate into the result type
*/

% Bare function calls (substitute type decl for fcn decl)
type b0 : fcn _ : int
var dst : int

% Should fail (typerefs are not valid expressions)
dst := b0

%%% args: -b
%%% expected exit status: 255
%%% expected stdout:
 
%%% expected stderr:
%%% warn line:7 column:15-16 Function type declarations must specifiy '()' after the identifier
%%% error line:11 column:8-10 Expression is a type reference, and cannot be used here
