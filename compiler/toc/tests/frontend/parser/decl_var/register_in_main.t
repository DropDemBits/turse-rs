% Register declarations can't be in the main or global scope
var register a := 1
var pervasive register a := 1

%%% args: --only_parser -dump ast -b
%%% expected exit status: 255

%%% expected stdout:
%%% ast: [
%%% var register [id:0] := nat(1)
%%% var register [id:1] := nat(1)
%%% ]

%%% expected stderr:
%%% error line:2 column:5-13 'var' register bindings are not allowed in the main, module, monitor, or class level
%%% error line:3 column:15-23 'var' register bindings are not allowed in the main, module, monitor, or class level