% Folds should chain together
var a : int := 1 - 1 - 1 - 1 - 1

%%% args: -M -dump ast -b
%%% expected exit status: 0

%%% expected stdout:
%%% ast: {
%%%     var [id:0] : { prim Int } := int(-3)
%%% }

%%% expected stderr: