% Invalid forms - comma after last identifier
var a, b, c, : int := 5
var a, : int := 5

%%% args: --only_parser -dump ast -b
%%% expected exit status: 255

%%% expected stdout:
%%% ast: [
%%% var [id:0, id:1, id:2] : { prim Int } := nat(5)
%%% var [id:3] : { prim Int } := nat(5)
%%% ]

%%% expected stderr:
%%% error line:2 column:14-15 Expected an identifier after the comma
%%% error line:3 column:8-9 Expected an identifier after the comma