% Ensure that the constant folder preserves assignment semantics
var a : char(6) := 'abcd' + 'aaa'
var b : string(4) := 'abcd' + 'aaa'

%%% args: -M -dump ast -b
%%% expected exit status: 255

%%% expected stdout:
%%% ast: [
%%% var [id:0] : { char(nat(6)) } := "abcdaaa"
%%% var [id:1] : { string(nat(4)) } := "abcdaaa"
%%% ]

%%% expected stderr:
%%% error line:2 column:20-34 Initialization value is the wrong type
%%% error line:3 column:22-36 Initialization value is the wrong type