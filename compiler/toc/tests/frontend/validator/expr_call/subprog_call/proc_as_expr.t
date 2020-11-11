% Result type compat (proc not allowed as an expr)
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

var vi0 : int
var vc0 : char

% Result type compat (all should error, proc calls not allowed in expr position)
var store : int := vp0
store := vp0()
store := vp1()
store := vp2(1, "asd", 'c')
store := vp3('c')
store := vp4(vi0)
store := vp5(vc0)

%%% args: -b
%%% expected exit status: 255

%%% expected stderr:
%%% error line:20 column:20-23 Initialization value is the wrong type
%%% error line:21 column:10-13 Reference is to a procedure and cannot be used in expressions
%%% error line:22 column:10-13 Reference is to a procedure and cannot be used in expressions
%%% error line:23 column:10-13 Reference is to a procedure and cannot be used in expressions
%%% error line:24 column:10-13 Reference is to a procedure and cannot be used in expressions
%%% error line:25 column:10-13 Reference is to a procedure and cannot be used in expressions
%%% error line:26 column:10-13 Reference is to a procedure and cannot be used in expressions
