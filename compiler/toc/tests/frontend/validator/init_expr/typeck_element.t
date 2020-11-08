% Types should match field/element type (and not be a type reference)
% TODO: add tests for initializing union & record fields

% Reals
var re : array 1 .. 3 of int := init(1, 2.0, 3)
% Strings
var str : array 1 .. 3 of int := init(1, 2, "ee")
% Char
var ch : array 1 .. 3 of int := init('c', 2, 3)
% Char(*)
var chn : array 1 .. 3 of int := init('cde', 2, 3)
% Tyref
type tyref : int
var ty : array 1 .. 3 of int := init(tyref, 2, 3)

%%% args: -M -b

%%% expected stderr:
%%% error line:5 column:41-44 Initializer value evaluates to the wrong type
%%% error line:7 column:45-49 Initializer value evaluates to the wrong type
%%% error line:9 column:38-41 Initializer value evaluates to the wrong type
%%% error line:11 column:39-44 Initializer value evaluates to the wrong type
%%% error line:14 column:38-43 Reference does not refer to a variable or constant
%%% error line:14 column:38-43 Initializer value evaluates to the wrong type