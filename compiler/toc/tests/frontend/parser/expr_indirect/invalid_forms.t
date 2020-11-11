% Expect '(' after '@'
var a := int @ 0)

% Expect '(' after expr
var a := int @ 0
begin end % sync
var a := int @ (0
begin end % sync

% Address expression should be empty in these cases
var a := int @ ()
var a := int @
begin end % sync

% Should not be a bare error expr
var a := int @ (+)

%%% args: --only_parser -dump ast -b
%%% expected exit status: 255

%%% expected stdout:
%%% ast: {
%%%     var [id:0] := [{ prim Int }] @ (nat(0))
%%%     var [id:1] := [{ prim Int }] @ (nat(0))
%%%     {}
%%%     var [id:2] := [{ prim Int }] @ (nat(0))
%%%     {}
%%%     var [id:3] := [{ prim Int }] @ (<error>)
%%%     var [id:4] := [{ prim Int }] @ (<error>)
%%%     {}
%%%     var [id:5] := [{ prim Int }] @ (+<error>)
%%% }

%%% expected stderr:
%%% error line:2 column:16-17 Expected '(' after '@'
%%% error line:5 column:16-17 Expected '(' after '@'
%%% error line:6 column:1-6 Expected ')' after address expression
%%% error line:8 column:1-6 Expected ')' after address expression
%%% error line:11 column:17-18 Expected expression before ')' 
%%% error line:13 column:1-6 Expected '(' after '@'
%%% error line:13 column:1-6 Expected expression before 'begin' 
%%% error line:13 column:1-6 Expected ')' after address expression
%%% error line:16 column:18-19 Expected expression before ')'
