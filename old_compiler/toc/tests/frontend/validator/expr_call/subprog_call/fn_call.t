/*
 - Function call
   - Result type compat (proc not allowed as an expr)
   - Too few parameters
   - Too many parameters
   - Type ref
   - var for ref param
   - const expr for ref param
   - other for ref param
   - empty for ref param
   - wrong type no coerece
   - wrong type no coerece empty
   - wrong type coerce
*/

% Predefs
var vi0 : int
var vc0 : char

% Fcn set
% no *f0 without func decl
type f1 : fcn _ () : int
type f2 : fcn _ (a : int, b : string, c : char) : int
type f3 : fcn _ (a : cheat int) : int
type f4 : fcn _ (var a : int) : int
type f5 : fcn _ (var a : cheat int) : int

var vf1 : f1
var vf2 : f2
var vf3 : f3
var vf4 : f4
var vf5 : f5

% Result type compat (no errors)
var res : int := vf1()
res := vf2(1, "asd", 'c')
res := vf3('c')
res := vf4(vi0)
res := vf5(vc0)

%%% args: -b
%%% expected exit status: 0
%%% expected stderr:
%%% 
