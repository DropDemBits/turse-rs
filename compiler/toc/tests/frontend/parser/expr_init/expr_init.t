% Size checking & compile-time checking is performed by the validator
var a : array 1 .. 3 of int := init(1, 2, 3)
var a : array 1 .. * of int := init(1)

%%% args: --only_parser -dump ast -b
%%% expected exit status: 0

%%% expected stdout:
%%% ast: {
%%%     var [id:0] : { array { range nat(1) .. nat(3) } of { prim Int } } := init(nat(1), nat(2), nat(3))
%%%     var [id:1] : { array { range nat(1) .. * } of { prim Int } } := init(nat(1))
%%% }

%%% expected stderr:
