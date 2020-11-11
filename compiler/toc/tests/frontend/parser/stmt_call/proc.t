% Calling procedure with no args with and without parens
% TODO: Check for calls with reference behind dot, arrow, and deref exprs

% Error is captured by Validator
var p : proc _ ()
p
p()

% Calling procedure with an arg
var q : proc _ (a : int)
q(1)

%%% args: --only_parser -dump ast -b
%%% expected exit status: 0

%%% expected stdout:
%%% ast: {
%%%     var [id:0] : { procedure () }
%%%     ref(id:0)()
%%%     ref(id:0)()
%%%     var [id:1] : { procedure (a : { prim Int }) }
%%%     ref(id:1)(nat(1))
%%% }

%%% expected stderr:
