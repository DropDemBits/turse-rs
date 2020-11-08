% Ensure that the constant folder preserves assignment semantics
var a : char(6) := 'abcd' + 'aaa'

%%% args: -M -dump ast -b
%%% expected exit status: 255

%%% expected stdout:
%%% ast: [
%%% var [id:0] : ty_prim[CharN(Size(6))] := "abcdaaa"
%%% ]

%%% expected stderr:
%%% error line:2 column:20-34 Initialization value is the wrong type