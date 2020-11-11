% Valid forms
var a : int := 1
var b : int
var c := 3 + 6 ** 2
var d, e, f : string := "hai"
var x, y, z : real := 42e10

% Accepted forms
var g : int = -5
var h : int = -10 + 3 * 2
var i, j, k : nat = 20 + 40 shl 5

%%% args: --only_parser -dump ast -b
%%% expected exit status: 0

%%% expected stdout:
%%% ast: [
%%% var [id:0] : { prim Int } := nat(1)
%%% var [id:1] : { prim Int }
%%% var [id:2] := nat(3) + nat(6) ** nat(2)
%%% var [id:3, id:4, id:5] : { prim String_ } := "hai"
%%% var [id:6, id:7, id:8] : { prim Real } := real(420000000000)
%%% var [id:9] : { prim Int } := -nat(5)
%%% var [id:10] : { prim Int } := -nat(10) + nat(3) * nat(2)
%%% var [id:11, id:12, id:13] : { prim Nat } := nat(20) + nat(40) shl nat(5)
%%% ]

%%% expected stderr:
%%% warn line:9 column:13-14 '=' found, assumed it to be ':='
%%% warn line:10 column:13-14 '=' found, assumed it to be ':='
%%% warn line:11 column:19-20 '=' found, assumed it to be ':='