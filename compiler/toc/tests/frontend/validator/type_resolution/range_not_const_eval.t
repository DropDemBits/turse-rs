% Range end bound must be a compile-time expression (in these contexts)
var eb : int := 1

% As set range
type set_range : set of 1 .. eb
% As range alias
type range_alias : 1 .. eb
% As range restriction
var range_restrict : 1 .. eb
% As part of an array range
type array_range : array 1 .. eb of int

%%% args: -M -b
%%% expected exit status: 255

%%% expected stdout:

%%% expected stderr:
%%% error line:5 column:30-32 End bound must be a compile-time expression
%%% error line:7 column:25-27 End bound must be a compile-time expression
%%% error line:9 column:27-29 End bound must be a compile-time expression
%%% error line:11 column:31-33 End bound must be a compile-time expression