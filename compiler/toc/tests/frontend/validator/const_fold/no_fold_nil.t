% 'nil' should never be folded
var k : nat := #nil

%%% args: -M -dump ast -b
%%% expected exit status: 0

%%% expected stdout:
%%% ast: [
%%% var [id:0] : ty_prim[Nat] := #nil
%%% ]

%%% expected stderr:
