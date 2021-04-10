% Function expects ':' before result type
var a : function a int

% Function expects type after ':'
var a : function a :

% Function type declaration expects '()' if there are no parameters (only as a warning)
var a : function amphy : int

% Function / procedure expects identifier after keyword (this can be made optional in the future)
var a : procedure
begin end

var a : function : int
begin end

%%% args: --only_parser -dump ast -b
%%% expected exit status: 255

%%% expected stdout:
%%% ast: {
%%%     var [id:0] : { function -> { prim Int } }
%%%     var [id:1] : { function -> { error } }
%%%     var [id:2] : { function -> { prim Int } }
%%%     var [id:3] : { procedure }
%%%     {}
%%%     var [id:4] : { function -> { prim Int } }
%%%     {}
%%% }

%%% expected stderr:
%%% error line:2 column:20-23 Expected ':' before the result type
%%% error line:8 column:1-4 Expected expression before 'var' 
%%% error line:8 column:1-4 Unexpected 'var', expected a type specifier
%%% error line:12 column:1-6 Expected identifier in function type declaration
%%% error line:14 column:18-19 Expected identifier in function type declaration
%%% warn line:2 column:18-19 Function type declarations must specifiy '()' after the identifier
%%% warn line:5 column:18-19 Function type declarations must specifiy '()' after the identifier
%%% warn line:8 column:18-23 Function type declarations must specifiy '()' after the identifier
%%% warn line:14 column:9-17 Function type declarations must specifiy '()' after 'function'