% Range bounds can't be reals, or any other types
type a : 0.0 .. 0.0
type b : 0.0 + 0.0 .. 0.0 + 0.0

%%% args: -M -b
%%% expected exit status: 255

%%% expected stdout:

%%% expected stderr:
%%% error line:2 column:10-20 Range bounds must both be integers, characters, booleans, or elements from the same enumeration
%%% error line:3 column:10-32 Range bounds must both be integers, characters, booleans, or elements from the same enumeration