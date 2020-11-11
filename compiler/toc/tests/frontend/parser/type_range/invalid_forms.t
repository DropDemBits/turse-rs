% Inferred range end is only valid in array range contexts
var a : 1 .. *

% No range end
var a : 1 .. 
begin end

% No range end in function parameter
var a : proc _ (a : array 1 .. )

%%% args: --only_parser -dump ast -b
%%% expected exit status: 255

%%% expected stdout:
%%% ast: [
%%% var [id:0] : { range nat(1) .. * }
%%% var [id:1] : { range nat(1) .. <error> }
%%% {}
%%% var [id:2] : { procedure (a : { array { range nat(1) .. <error> } of { error } }) }
%%% ]

%%% expected stderr:
%%% error line:2 column:14-15 '*' as a range end is only valid in an array range
%%% error line:5 column:11-13 Expected expression after '..'
%%% error line:6 column:1-6 Expected expression before 'begin' 
%%% error line:9 column:29-31 Expected expression or '*' after '..'
%%% error line:9 column:32-33 Expected expression before ')' 
%%% error line:9 column:32-33 Expected 'of' after the last range specifier
%%% error line:9 column:32-33 Expected expression before ')' 
%%% error line:9 column:32-33 Unexpected ')', expected a type specifier