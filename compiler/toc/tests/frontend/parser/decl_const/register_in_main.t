% Register declarations can't be in the main or global scope
const register a := 1
const pervasive register a := 1

%%% args: --only_parser -dump ast -b
%%% expected exit status: 255

%%% expected stdout:
%%% ast: {
%%%     const register [id:0] := nat(1)
%%%     const register [id:1] := nat(1)
%%% }

%%% expected stderr:
%%% error line:2 column:7-15 'const' register bindings are not allowed in the main, module, monitor, or class level
%%% error line:3 column:17-25 'const' register bindings are not allowed in the main, module, monitor, or class level