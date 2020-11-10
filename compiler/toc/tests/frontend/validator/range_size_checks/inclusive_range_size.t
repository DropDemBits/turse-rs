% Ranges in Turing are inclusive on both bounds

var a : 1 .. 16

const s : int := 0
const e : int := 10
var b : s .. e

%%% args: -M -dump types -b
%%% expected exit status: 0

%%% expected stdout:
%%% types: [
%%%        0 -> { range 1 .. 16 (16) ty_prim[Int] }
%%%        1 -> { range 0 .. 10 (11) ty_prim[Int] }
%%% ]

%%% expected stderr:
