% No type refs allowed for unary or binary expressions
type a : int

var b : int
b := a + b
b := b + a
b += a
b := #a

%%% args: -M -b
%%% expected exit status: 255

%%% expected stderr:
%%% error line:5 column:6-7 Operand is not a variable or constant reference
%%% error line:6 column:10-11 Operand is not a variable or constant reference
%%% error line:8 column:7-8 Operand is not a variable or constant reference