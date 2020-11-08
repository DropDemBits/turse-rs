% All expressions in 'init' must be compile-time expressions
var a : array 1 .. * of int := init(1, 2, 3)
var b : array 1 .. * of int := init(1, 2 + 3 - 5 * 10, 3)

const c := 5
var d : array 1 .. * of int := init(1, c - 5, 3)

%%% args: -M -b
%%% expected exit status: 0

%%% expected stderr:
