/*
 - Bare Function call
   - Should always evaluate into the result type
*/

% Bare function calls (substitute type decl for fcn decl)
type b0 : fcn _ : int
var vb0 : b0
var dst : int

% Bare & with empty parens should work
dst := vb0
dst := vb0()

% Inside exprs should work
dst := vb0 + 1
dst := vb0() + 1

% Bare calls should work outside exprs
vb0
vb0()

%%% args: -b
%%% expected exit status: 0
%%% expected stdout:
 
%%% expected stderr:
%%% warn line:7 column:15-16 Function type declarations must specifiy '()' after the identifier