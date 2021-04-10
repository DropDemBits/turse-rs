% Calling fn with args
% TODO: Check for calls with reference behind dot, arrow, and deref exprs
var fn : fcn _ (a : int) : int

% Can be called in both statement position and reference position
fn(1)
var a := fn(1)

%%% args: --only_parser -dump ast -b
%%% expected exit status: 0

%%% expected stdout:
%%% ast: {
%%%     var [id:0] : { function (a : { prim Int }) -> { prim Int } }
%%%     ref(id:0)(nat(1))
%%%     var [id:1] := ref(id:0)(nat(1))
%%% }

%%% expected stderr:
