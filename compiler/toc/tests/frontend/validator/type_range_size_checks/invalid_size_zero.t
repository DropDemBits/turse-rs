% 0 sized ranges aren't valid anywhere else
type en : enum(a, b)

var a : array 1 .. 0 of int
var b : array 16#80000000 .. 16#7fffffff of int
var c : array true .. false of int
var d : array 'D' .. 'C' of int
var e : 16#80000000 .. 16#7fffffff
var f : true .. false
var g : 'D' .. 'C'
type h : set of 16#80000000 .. 16#7fffffff
type i : set of true .. false
type j : set of 'D' .. 'C'
var k : en.b .. en.a

begin
    const sb : en := en.b
    const eb : en := en.a
    var l : sb .. eb
end

begin
    const sb : int := 1
    const eb : int := 0
    var m : array sb .. eb of int
end

%%% args: -M -dump types -b
%%% expected exit status: 255

%%% expected stdout:
%%% types: [
%%%        0 -> { alias to ty_id[4] }
%%%        1 -> { alias to ty_id[19] }
%%%        2 -> { alias to ty_id[21] }
%%%        3 -> { alias to ty_id[23] }
%%%        4 -> { enum ( a(ty_id[5]) b(ty_id[6]) ) }
%%%        5 -> { enum_field(0) of ty_id[4] }
%%%        6 -> { enum_field(1) of ty_id[4] }
%%%        7 -> { range 1 .. 0 (0) ty_prim[Int] }
%%%        8 -> Array { ty_error of ty_prim[Int] }
%%%        9 -> { range 2147483648 .. 2147483647 (0) ty_prim[Int] }
%%%       10 -> Array { ty_error of ty_prim[Int] }
%%%       11 -> { range 1 .. 0 (0) ty_prim[Boolean] }
%%%       12 -> Array { ty_error of ty_prim[Int] }
%%%       13 -> { range 68 .. 67 (0) ty_prim[StringN(Size(1))] }
%%%       14 -> Array { ty_error of ty_prim[Int] }
%%%       15 -> { range 2147483648 .. 2147483647 (0) ty_prim[Int] }
%%%       16 -> { range 1 .. 0 (0) ty_prim[Boolean] }
%%%       17 -> { range 68 .. 67 (0) ty_prim[StringN(Size(1))] }
%%%       18 -> { range 2147483648 .. 2147483647 (0) ty_prim[Int] }
%%%       19 -> { set of ty_error }
%%%       20 -> { range 1 .. 0 (0) ty_prim[Boolean] }
%%%       21 -> { set of ty_error }
%%%       22 -> { range 68 .. 67 (0) ty_prim[StringN(Size(1))] }
%%%       23 -> { set of ty_error }
%%%       24 -> { range 1 .. 0 (0) ty_id[4] }
%%%       25 -> { range 1 .. 0 (0) ty_id[4] }
%%%       26 -> { range 1 .. 0 (0) ty_prim[Int] }
%%%       27 -> Array { ty_error of ty_prim[Int] }
%%% ]

%%% expected stderr:
%%% error line:4 column:15-21 Range bounds creates a zero-sized range
%%% error line:5 column:15-41 Range bounds creates a zero-sized range
%%% error line:6 column:15-28 Range bounds creates a zero-sized range
%%% error line:7 column:15-25 Range bounds creates a zero-sized range
%%% error line:8 column:9-35 Range bounds creates a zero-sized range
%%% error line:9 column:9-22 Range bounds creates a zero-sized range
%%% error line:10 column:9-19 Range bounds creates a zero-sized range
%%% error line:11 column:17-43 Range bounds creates a zero-sized range
%%% error line:12 column:17-30 Range bounds creates a zero-sized range
%%% error line:13 column:17-27 Range bounds creates a zero-sized range
%%% error line:14 column:9-21 Range bounds creates a zero-sized range
%%% error line:19 column:13-21 Range bounds creates a zero-sized range
%%% error line:25 column:19-27 Range bounds creates a zero-sized range