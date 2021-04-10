% Valid forms
var a : int := 1
var b : int
var c := 3 + 6 ** 2
var d, e, f : string := "hai"
var x, y, z : real := 42e10

var * a : int := 1
var pervasive a : int := 1

begin
    % Register declarations can only be contained in deeper scopes
    var register a := 1
    var pervasive register a := 1
end

% Accepted forms
var g : int = -5
var h : int = -10 + 3 * 2
var i, j, k : nat = 20 + 40 shl 5

%%% args: --only_parser -dump ast -dump scope -b
%%% expected exit status: 0

%%% expected stdout:
%%% ast: {
%%%     var [id:0] : { prim Int } := nat(1)
%%%     var [id:1] : { prim Int }
%%%     var [id:2] := nat(3) + nat(6) ** nat(2)
%%%     var [id:3, id:4, id:5] : { prim String_ } := "hai"
%%%     var [id:6, id:7, id:8] : { prim Real } := real(420000000000)
%%%     var [id:9] : { prim Int } := nat(1)
%%%     var [id:10] : { prim Int } := nat(1)
%%%     {
%%%         var register [id:11] := nat(1)
%%%         var register [id:12] := nat(1)
%%%     }
%%%     var [id:13] : { prim Int } := -nat(5)
%%%     var [id:14] : { prim Int } := -nat(10) + nat(3) * nat(2)
%%%     var [id:15, id:16, id:17] : { prim Nat } := nat(20) + nat(40) shl nat(5)
%%% }
%%% scope: [
%%%        0 -> { a ty: ty_unknown, used: 0, var decl }
%%%        1 -> { b ty: ty_unknown, used: 0, var decl }
%%%        2 -> { c ty: ty_unknown, used: 0, var decl }
%%%        3 -> { d ty: ty_unknown, used: 0, var decl }
%%%        4 -> { e ty: ty_unknown, used: 0, var decl }
%%%        5 -> { f ty: ty_unknown, used: 0, var decl }
%%%        6 -> { x ty: ty_unknown, used: 0, var decl }
%%%        7 -> { y ty: ty_unknown, used: 0, var decl }
%%%        8 -> { z ty: ty_unknown, used: 0, var decl }
%%%        9 -> { a ty: ty_unknown, used: 0, var decl pervasive }
%%%       10 -> { a ty: ty_unknown, used: 0, var decl pervasive }
%%%       11 -> { a ty: ty_unknown, used: 0, var decl }
%%%       12 -> { a ty: ty_unknown, used: 0, var decl pervasive }
%%%       13 -> { g ty: ty_unknown, used: 0, var decl }
%%%       14 -> { h ty: ty_unknown, used: 0, var decl }
%%%       15 -> { i ty: ty_unknown, used: 0, var decl }
%%%       16 -> { j ty: ty_unknown, used: 0, var decl }
%%%       17 -> { k ty: ty_unknown, used: 0, var decl }
%%% ]

%%% expected stderr:
%%% warn line:18 column:13-14 '=' found, assumed it to be ':='
%%% warn line:19 column:13-14 '=' found, assumed it to be ':='
%%% warn line:20 column:19-20 '=' found, assumed it to be ':='