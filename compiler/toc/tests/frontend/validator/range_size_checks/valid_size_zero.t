% 0 sized ranges are valid in some contexts (e.g. flexible arrays)
type e : enum(a, b)

var arr : flexible array 16#8000000000000000 .. 16#7fffffffffffffff of int
var arb : flexible array true .. false of int
var arc : flexible array 'D' .. 'C' of int
var are : flexible array e.b .. e.a of int

begin
    const sb : e := e.b
    const eb : e := e.a
    var afc : flexible array sb .. eb of int
end

begin
    const sb : int := 1
    const eb : int := 0
    var afc : flexible array sb .. eb of int
end

%%% args: -M -dump types -b
%%% expected exit status: 0

%%% expected stdout:
%%% types: [
%%%        0 -> { enum ( a(ty_id[1]) b(ty_id[2]) )
%%%        1 -> { enum_field(0) of ty_id[0] }
%%%        2 -> { enum_field(1) of ty_id[0] }
%%%        3 -> { alias to ty_id[0] }
%%%        4 -> { range nat(9223372036854775808) .. nat(9223372036854775807) (0) ty_prim[LongInt] }
%%%        5 -> Array { flexible ty_id[4] of ty_prim[Int] }
%%%        6 -> { range bool(true) .. bool(false) (0) ty_prim[Boolean] }
%%%        7 -> Array { flexible ty_id[6] of ty_prim[Int] }
%%%        8 -> { range "D" .. "C" (0) ty_prim[StringN(Size(1))] }
%%%        9 -> Array { flexible ty_id[8] of ty_prim[Int] }
%%%       10 -> { range nat(1) .. nat(0) (0) ty_id[0] }
%%%       11 -> Array { flexible ty_id[10] of ty_prim[Int] }
%%%       12 -> { ref_expr ref(id:0) }
%%%       13 -> { ref_expr ref(id:0) }
%%%       14 -> { range nat(1) .. nat(0) (0) ty_id[0] }
%%%       15 -> Array { flexible ty_id[14] of ty_prim[Int] }
%%%       16 -> { range nat(1) .. nat(0) (0) ty_prim[Int] }
%%%       17 -> Array { flexible ty_id[16] of ty_prim[Int] }
%%% ]

%%% expected stderr:
