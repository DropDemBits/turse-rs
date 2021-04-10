% Range bounds types do not match
type a : true .. 'c'
type b : 1 .. 'c'
type c : 'c' .. true
type d : 'c' .. 'aa'
type e : 'cb' .. 'aa'

%%% args: -M -b
%%% expected exit status: 255

%%% expected stdout:

%%% expected stderr:
%%% error line:2 column:10-21 Range bounds must be both integers, characters, booleans, or elements from the same enumeration
%%% error line:3 column:10-18 Range bounds must be both integers, characters, booleans, or elements from the same enumeration
%%% error line:4 column:10-21 Range bounds must be both integers, characters, booleans, or elements from the same enumeration
%%% error line:5 column:10-21 Range bounds must be both integers, characters, booleans, or elements from the same enumeration
%%% error line:6 column:10-22 Range bounds must be both integers, characters, booleans, or elements from the same enumeration