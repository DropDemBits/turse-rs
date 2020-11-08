
% Aliases of resolved forward types are equivalent to their base types
type a : forward
type a : int
type b : a
var c : a := 2

%%% args: -M -b
%%% expected exit status: 0

%%% expected stdout:

%%% expected stderr:
