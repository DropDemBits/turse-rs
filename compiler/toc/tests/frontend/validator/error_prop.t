% From fuzzing, should not panic
type : a
begin a end

var k
var l : k
begin l end

const i:char(Qbegin v
const i:char(egin begin+egin

%%% args: -M -b
%%% expected exit status: 255

%%% expected stderr:
%%% error line:2 column:6-7 Expected identifier after 'type'
%%% error line:2 column:8-9 'a' has not been declared yet
%%% error line:2 column:8-9 'a' does not refer to a type
%%% error line:3 column:7-8 'a' has not been declared yet
%%% error line:3 column:7-8 'a' cannot be called or have subscripts
%%% error line:5 column:1-4 Cannot infer type for given var declaration (no type specification or initial value given)
%%% error line:6 column:9-10 'k' does not refer to a type
%%% error line:7 column:7-8 'l' cannot be called or have subscripts
%%% error line:9 column:1-6 const declaration requires an initial value
%%% error line:9 column:21-22 Expected ')' after length specifier
%%% error line:9 column:21-22 'v' has not been declared yet
%%% error line:9 column:21-22 'v' cannot be called or have subscripts
%%% error line:10 column:1-6 const declaration requires an initial value
%%% error line:10 column:7-8 'i' has already been declared
%%% error line:10 column:19-24 Expected ')' after length specifier
%%% error line:10 column:19-24 'begin' block does not have a matching 'end'
%%% error line:10 column:24-25 '+' does not begin a statement or declaration