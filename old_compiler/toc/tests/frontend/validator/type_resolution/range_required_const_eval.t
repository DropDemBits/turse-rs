% Range end bound must be a compile-time expression (in these contexts)

% As set range
type set_range : set of 1 .. (8 + 20 - 3)
% As range alias
type range_alias : 1 .. (8 + 20 - 3)
% As range restriction
var range_restrict : 1 .. (8 + 20 - 3)
% As part of an array range
type array_range : array 1 .. (8 + 20 - 3) of int

%%% args: -M -b
%%% expected exit status: 0

%%% expected stdout:

%%% expected stderr:
