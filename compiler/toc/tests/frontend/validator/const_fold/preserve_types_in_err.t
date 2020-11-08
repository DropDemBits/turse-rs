% Preserve types in a folding error
var a : int := 1 + 0.1 - 1 - 0.1 - 1 - 1

%%% args: -M -dump ast -b
%%% expected exit status: 255

%%% expected stdout:
%%% ast: [
%%% var [id:0] : ty_prim[Int] := real(-2)
%%% ]

%%% expected stderr:
%%% error line:2 column:16-41 Initialization value is the wrong type