% Folding should stop at runtime evaluations
var a : nat := 1
var b := a + (1 + 1)

%%% args: -M -dump ast -b
%%% expected exit status: 0

%%% expected stdout:
%%% ast: {
%%%     var [id:0] : { prim Nat } := nat(1)
%%%     var [id:1] := ref(id:0) + (nat(1) + nat(1))
%%% }

%%% expected stderr:
