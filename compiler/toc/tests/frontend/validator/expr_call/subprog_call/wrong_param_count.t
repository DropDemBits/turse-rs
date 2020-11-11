% Too few parameters & too many parameters
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

% Too few parameters
vp2(1, "asd")
vp2(1)
vp2()
vp3()
vp4()
vp5()

% Too many parameters
vp0(false)
vp1(false)
vp2(1, "asd", 'c', false)
vp3('c', false)
vp4(vi0, false)
vp5(vc0, false)

vp0(false, false)
vp1(false, false)
vp2(1, "asd", 'c', false, false)
vp3('c', false, false)
vp4(vi0, false, false)
vp5(vc0, false, false)

%%% args: -b
%%% expected exit status: 255

%%% expected stderr:
%%% error line:20 column:4-5 Missing 1 arguments for call expression
%%% error line:21 column:4-5 Missing 2 arguments for call expression
%%% error line:22 column:4-5 Missing 3 arguments for call expression
%%% error line:23 column:4-5 Missing 1 arguments for call expression
%%% error line:24 column:4-5 Missing 1 arguments for call expression
%%% error line:25 column:4-5 Missing 1 arguments for call expression
%%% error line:28 column:4-5 1 extra arguments for call expression
%%% error line:29 column:4-5 1 extra arguments for call expression
%%% error line:30 column:4-5 1 extra arguments for call expression
%%% error line:31 column:4-5 1 extra arguments for call expression
%%% error line:32 column:4-5 1 extra arguments for call expression
%%% error line:33 column:4-5 1 extra arguments for call expression
%%% error line:35 column:4-5 2 extra arguments for call expression
%%% error line:36 column:4-5 2 extra arguments for call expression
%%% error line:37 column:4-5 2 extra arguments for call expression
%%% error line:38 column:4-5 2 extra arguments for call expression
%%% error line:39 column:4-5 2 extra arguments for call expression
%%% error line:40 column:4-5 2 extra arguments for call expression
