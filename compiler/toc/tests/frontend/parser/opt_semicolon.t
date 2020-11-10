% Optional trailing semicolons are ignored
;;;;;
var a : int := 1;
;;;;;var b : int := 1;

%%% args: --only_parser -dump ast -b
%%% expected exit status: 0

%%% expected stdout:
%%% ast: [
%%% var [id:0] : { prim Int } := nat(1)
%%% var [id:1] : { prim Int } := nat(1)
%%% ]

%%% expected stderr:
