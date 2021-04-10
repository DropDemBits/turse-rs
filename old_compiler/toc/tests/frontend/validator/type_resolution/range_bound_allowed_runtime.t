% Range end bound is allowed to be a runtime expression (in this context)

% As part of a dynamic array
var a : int := 1
var b : array 1 .. a of int

%%% args: -M -b
%%% expected exit status: 0

%%% expected stdout:

%%% expected stderr:
