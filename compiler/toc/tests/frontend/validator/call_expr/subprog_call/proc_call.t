/*
 - Procedure call
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

type p0 : proc _
type p1 : proc _ ()
type p2 : proc _ (a : int, b : string, c : char)
type p3 : proc _ (a : cheat int)
type p4 : proc _ (var a : int)
type p5 : proc _ (var a : cheat int)

var vp0 : p0
var vp1 : p1
var vp2 : p2
var vp3 : p3
var vp4 : p4
var vp5 : p5

% Proc set (no errors)
vp0
vp0()
vp1()
vp2(1, "asd", 'c')
vp3('c')
vp4(vi0)
vp5(vc0)

%%% args: -b
%%% expected exit status: 0
%%% expected stderr:
%%% 
