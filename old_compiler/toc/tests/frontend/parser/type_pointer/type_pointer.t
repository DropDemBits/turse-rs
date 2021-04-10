var a : pointer to int
var a_alt : unchecked pointer to int
var b : ^ string

%%% args: --only_parser -dump ast -b
%%% expected exit status: 0

%%% expected stdout:
%%% ast: {
%%%     var [id:0] : { pointer to { prim Int } }
%%%     var [id:1] : { unchecked pointer to { prim Int } }
%%%     var [id:2] : { pointer to { prim String_ } }
%%% }

%%% expected stderr:
