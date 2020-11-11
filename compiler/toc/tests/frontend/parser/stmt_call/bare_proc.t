% Calling bare procedure with and without parens
% TODO: Check for calls with reference behind dot, arrow, and deref exprs
var p : proc _
p
p()

%%% args: --only_parser -dump ast -b
%%% expected exit status: 0

%%% expected stdout:
%%% ast: {
%%%     var [id:0] : { procedure }
%%%     ref(id:0)()
%%%     ref(id:0)()
%%% }

%%% expected stderr:
