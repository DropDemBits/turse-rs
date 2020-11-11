% Using == instead of = for equality comparison
% Should report
var q := a == true

%%% args: --only_parser -dump ast -b
%%% expected exit status: 255

%%% expected stdout:
%%% ast: [
%%% var [id:1] := ref(id:0) = <error> = bool(true)
%%% ]

%%% expected stderr:
%%% error line:3 column:13-14 Expected expression before '=' (Did you mean '=' instead of '=='?)