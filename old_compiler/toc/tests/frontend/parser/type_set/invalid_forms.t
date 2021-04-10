% Set type declarations are only valid in type statements
var a : set of 1 .. 3
const a : set of 1 .. 3 := 1

% Set type declarations expect 'of'
type a : set 1 .. 3
type a : set

% Set type declarations expect a range
type a : set of 
begin end

%%% args: --only_parser -dump ast -b
%%% expected exit status: 255

%%% expected stdout:
%%% ast: {
%%%     var [id:0] : { set of { range nat(1) .. nat(3) } }
%%%     const [id:1] : { set of { range nat(1) .. nat(3) } } := nat(1)
%%%     type [id:2] : { set of { range nat(1) .. nat(3) } }
%%%     type [id:3] : { set of { error } }
%%%     type [id:4] : { set of { error } }
%%%     {}
%%% }

%%% expected stderr:
%%% error line:2 column:9-22 Set types can only be declared inside of 'type' statements
%%% error line:3 column:11-24 Set types can only be declared inside of 'type' statements
%%% error line:6 column:14-15 Expected 'of' after 'set'
%%% error line:10 column:1-5 Expected 'of' after 'set'
%%% error line:10 column:1-5 Expected expression before 'type' 
%%% error line:11 column:1-6 Expected expression before 'begin'
