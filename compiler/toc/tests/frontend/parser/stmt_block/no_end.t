% Missing `end`
begin
    var yay : int := 5

var yay : string := "hello!"

%%% args: --only_parser -dump ast -b
%%% expected exit status: 255

%%% expected stdout:
%%% ast: [
%%% {
%%%     var [id:0] : { prim Int } := nat(5)
%%%     var [id:1] : { prim String_ } := "hello!"
%%% }
%%% ]

%%% expected stderr:
%%% error line:2 column:1-6 'begin' block does not have a matching 'end'