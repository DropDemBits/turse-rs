% Pull compile-time expressions from inside grouping expressions
const c := (0)
const d := (1 + 1)

%%% args: -M -dump ast -b
%%% expected exit status: 0

%%% expected stdout:
%%% ast: [
%%% const [id:0] := nat(0)
%%% const [id:1] := nat(2)
%%% ]

%%% expected stderr:
