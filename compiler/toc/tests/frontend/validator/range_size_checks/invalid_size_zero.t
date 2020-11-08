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
%%%        0 -> { enum ( a(ty_id[1]) b(ty_id[2]) )
%%%        1 -> { enum_field(0) of ty_id[0] }
%%%        2 -> { enum_field(1) of ty_id[0] }
%%%        3 -> { alias to ty_id[0] }
%%%        4 -> { range nat(1) .. nat(0) (0) ty_prim[Int] }
%%%        5 -> Array { ty_error of ty_prim[Int] }
%%%        6 -> { range nat(2147483648) .. nat(2147483647) (0) ty_prim[Int] }
%%%        7 -> Array { ty_error of ty_prim[Int] }
%%%        8 -> { range bool(true) .. bool(false) (0) ty_prim[Boolean] }
%%%        9 -> Array { ty_error of ty_prim[Int] }
%%%       10 -> { range "D" .. "C" (0) ty_prim[StringN(Size(1))] }
%%%       11 -> Array { ty_error of ty_prim[Int] }
%%%       12 -> { range nat(2147483648) .. nat(2147483647) (0) ty_prim[Int] }
%%%       13 -> { range bool(true) .. bool(false) (0) ty_prim[Boolean] }
%%%       14 -> { range "D" .. "C" (0) ty_prim[StringN(Size(1))] }
%%%       15 -> { range nat(2147483648) .. nat(2147483647) (0) ty_prim[Int] }
%%%       16 -> { set of ty_error }
%%%       17 -> { alias to ty_id[16] }
%%%       18 -> { range bool(true) .. bool(false) (0) ty_prim[Boolean] }
%%%       19 -> { set of ty_error }
%%%       20 -> { alias to ty_id[19] }
%%%       21 -> { range "D" .. "C" (0) ty_prim[StringN(Size(1))] }
%%%       22 -> { set of ty_error }
%%%       23 -> { alias to ty_id[22] }
%%%       24 -> { range nat(1) .. nat(0) (0) ty_id[0] }
%%%       25 -> { ref_expr ref(id:0) }
%%%       26 -> { ref_expr ref(id:0) }
%%%       27 -> { range nat(1) .. nat(0) (0) ty_id[0] }
%%%       28 -> { range nat(1) .. nat(0) (0) ty_prim[Int] }
%%%       29 -> Array { ty_error of ty_prim[Int] }
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