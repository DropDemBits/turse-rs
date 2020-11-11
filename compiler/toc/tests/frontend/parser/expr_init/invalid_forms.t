% Expect at least one expression
var a : array 1 .. * of int := init() begin end

% Expect closing paren
var a : array 1 .. * of int := init( begin end

% Expect starting paren
var a : array 1 .. * of int := init) begin end

% Expect parens
var a : array 1 .. * of int := init begin end

% Expect expr after comma (length 2)
var a : array 1 .. * of int := init(1,) begin end

% Expect expr after comma (length 3)
var a : array 1 .. * of int := init(1,,) begin end

% Bad exprs should still contribute to length
var a : array 1 .. * of int := init(1,+,+,4) begin end

% Can only be used in initalization of const's & var's
var a : array 1 .. 3 of int
a := init(1,2,3)
a := +init(1,2,3)
a := -init(1,2,3)
a := init(1,2,3) + init(1,2,3)

% Arrays require init initializers
var a : array 1 .. * of int

% Can't infer type from init
var a := init(1, 2, 3)
const a := init(1, 2, 3)

%%% args: --only_parser -dump ast -b
%%% expected exit status: 255

%%% expected stdout:
%%% ast: {
%%%     var [id:0] : { array { range nat(1) .. * } of { prim Int } } := init(<error>)
%%%     {}
%%%     var [id:1] : { array { range nat(1) .. * } of { prim Int } } := init(<error>)
%%%     {}
%%%     var [id:2] : { array { range nat(1) .. * } of { prim Int } } := init(<error>)
%%%     {}
%%%     var [id:3] : { array { range nat(1) .. * } of { prim Int } } := init(<error>)
%%%     {}
%%%     var [id:4] : { array { range nat(1) .. * } of { prim Int } } := init(nat(1), <error>)
%%%     {}
%%%     var [id:5] : { array { range nat(1) .. * } of { prim Int } } := init(nat(1), <error>, <error>)
%%%     {}
%%%     var [id:6] : { array { range nat(1) .. * } of { prim Int } } := init(nat(1), +<error>, +<error>, nat(4))
%%%     {}
%%%     var [id:7] : { array { range nat(1) .. nat(3) } of { prim Int } }
%%%     ref(id:7) := init(nat(1), nat(2), nat(3))
%%%     ref(id:7) := +<error>
%%%     var [id:8] : { array { range nat(1) .. * } of { prim Int } }
%%%     var [id:9] : { error } := init(nat(1), nat(2), nat(3))
%%%     const [id:10] : { error } := init(nat(1), nat(2), nat(3))
%%% }

%%% expected stderr:
%%% error line:2 column:37-38 Expected expression before ')' 
%%% error line:5 column:38-43 Expected expression before 'begin' 
%%% error line:5 column:38-43 Expected ')' after the last expression
%%% error line:8 column:36-37 Expected '(' after 'init'
%%% error line:8 column:36-37 Expected expression before ')' 
%%% error line:11 column:37-42 Expected '(' after 'init'
%%% error line:11 column:37-42 Expected expression before 'begin' 
%%% error line:11 column:37-42 Expected ')' after the last expression
%%% error line:14 column:39-40 Expected expression before ')' 
%%% error line:17 column:39-40 Expected expression before ',' 
%%% error line:17 column:40-41 Expected expression before ')' 
%%% error line:20 column:40-41 Expected expression before ',' 
%%% error line:20 column:42-43 Expected expression before ',' 
%%% error line:24 column:6-10 'init' assignments are only valid in constant and variable declarations
%%% error line:25 column:7-11 Expected expression before 'init' 
%%% error line:25 column:7-11 'init' does not begin a statement or declaration
%%% error line:30 column:9-28 Arrays with '*' as an end bound require an 'init' initializer
%%% error line:33 column:10-23 Cannot infer a type from an 'init' initializer
%%% error line:34 column:12-25 Cannot infer a type from an 'init' initializer
