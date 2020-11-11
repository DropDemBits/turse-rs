% Valid forms
const a : int := 1
const b := 5.0
const c, d : int := 3
const e, f := 3 + 6 ** 2

% Accepted forms
const g : int = -5
const h : int = -10 + 3 * 2

%%% args: --only_parser -dump ast -b
%%% expected exit status: 0

%%% expected stdout:
%%% ast: [
%%% const [id:0] : { prim Int } := nat(1)
%%% const [id:1] := real(5)
%%% const [id:2, id:3] : { prim Int } := nat(3)
%%% const [id:4, id:5] := nat(3) + nat(6) ** nat(2)
%%% const [id:6] : { prim Int } := -nat(5)
%%% const [id:7] : { prim Int } := -nat(10) + nat(3) * nat(2)
%%% ]

%%% expected stderr:
%%% warn line:8 column:15-16 '=' found, assumed it to be ':='
%%% warn line:9 column:15-16 '=' found, assumed it to be ':='