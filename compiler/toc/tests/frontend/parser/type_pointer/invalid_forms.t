% Pointer type expects "to"
var a : pointer int
var a : unchecked pointer int

% Pointer type expects type
var a : ^                       begin end
var a : pointer                 begin end
var a : unchecked ^             begin end
var a : unchecked pointer       begin end

%%% args: --only_parser -dump ast -b
%%% expected exit status: 255

%%% expected stdout:
%%% ast: {
%%%     var [id:0] : { pointer to { prim Int } }
%%%     var [id:1] : { unchecked pointer to { prim Int } }
%%%     var [id:2] : { pointer to { error } }
%%%     {}
%%%     var [id:3] : { pointer to { error } }
%%%     {}
%%%     var [id:4] : { unchecked pointer to { error } }
%%%     {}
%%%     var [id:5] : { unchecked pointer to { error } }
%%%     {}
%%% }

%%% expected stderr:
%%% error line:2 column:17-20 Expected 'to' after 'pointer'
%%% error line:3 column:27-30 Expected 'to' after 'pointer'
%%% error line:6 column:33-38 Expected expression before 'begin' 
%%% error line:6 column:33-38 Unexpected 'begin', expected a type specifier
%%% error line:7 column:33-38 Expected 'to' after 'pointer'
%%% error line:7 column:33-38 Expected expression before 'begin' 
%%% error line:7 column:33-38 Unexpected 'begin', expected a type specifier
%%% error line:8 column:33-38 Expected expression before 'begin' 
%%% error line:8 column:33-38 Unexpected 'begin', expected a type specifier
%%% error line:9 column:33-38 Expected 'to' after 'pointer'
%%% error line:9 column:33-38 Expected expression before 'begin' 
%%% error line:9 column:33-38 Unexpected 'begin', expected a type specifier