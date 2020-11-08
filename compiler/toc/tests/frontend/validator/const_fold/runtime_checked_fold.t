% Valid type check, checked at runtime
var a : nat := (0 - 1)

%%% args: -M -dump ast -b
%%% expected exit status: 0

%%% expected stdout:
%%% ast: [
%%% var [id:0] : ty_prim[Nat] := int(-1)
%%% ]

%%% expected stderr:
