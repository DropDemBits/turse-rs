type ti : int
type tc : char
type ts : string

var vi0 : int
const ci0 : int := 3

type p2 : proc _ (a : int, b : string, c : char)
type p4 : proc _ (var a : int)

var vp2 : p2
var vp4 : p4

% Type ref parameters
vp2(ti, ts, tc)
vp4(ti)

% Var expr for ref param (no errors)
vp4(vi0)

% Const expr for ref param
vp4(ci0)

% Other expr for ref param
vp4(1 + 1 - 1 * 1 ** 1 div 1)

% Error expr for ref param
vp4(to)

% Wrong type, no coercion
vp2(vc0, vi0, vs0)
vp4(vs0)

% Wrong type, no corercion, Error
vp2(,,)
vp4(to)

% Wrong type, with corercion (no errors)
vp3('c')
vp5(vc0)

%%% args: -b
%%% expected exit status: 255

%%% expected stderr:
%%% error line:28 column:5-7 Expected expression before 'to' 
%%% error line:28 column:5-7 Missing ')' after parameter list
%%% error line:28 column:5-7 'to' does not begin a statement or declaration
%%% error line:15 column:5-7 Cannot use a type reference as a function or procedure parameter
%%% error line:15 column:9-11 Cannot use a type reference as a function or procedure parameter
%%% error line:15 column:13-15 Cannot use a type reference as a function or procedure parameter
%%% error line:16 column:5-7 Cannot use a type reference as a function or procedure parameter
%%% error line:22 column:5-8 Cannot pass a reference parameter to a constant reference
%%% error line:25 column:5-29 Cannot pass a reference parameter to an expression
%%% error line:28 column:5-7 Cannot pass a reference parameter to an expression
%%% error line:28 column:5-7 Argument is the wrong type
