var c := 5
var a : array 1 .. * of int := init(1, c, 3)

type d : int
var b : array 1 .. * of int := init(1, d, 3)

%%% args: -M -b

%%% expected stderr:
%%% error line:2 column:40-41 Expression is not a compile-time expression
%%% error line:5 column:40-41 Reference does not refer to a variable or constant
%%% error line:5 column:40-41 Initializer value evaluates to the wrong type