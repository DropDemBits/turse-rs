% Negative size ranges are invalid
var a : 16#80000000 .. 16#7ffffffe
var b : 16#ffffffffffffffff .. 16#fffffffffffffffd
var c : 'D' .. 'B'
var d : -1+3 .. 0

begin
    type e : enum(a, b, c)

    var f : e.c .. e.a

    const sb : e := e.c
    const eb : e := e.a
    var g : sb .. eb
end

%%% args: -M -dump types -b
%%% expected exit status: 255

%%% expected stdout:
%%% types: [
%%%        0 -> { range nat(2147483648) .. nat(2147483646)  ty_unknown }
%%%        1 -> { range nat(18446744073709551615) .. nat(18446744073709551613)  ty_unknown }
%%%        2 -> { range 'D' .. 'B'  ty_unknown }
%%%        3 -> { range -nat(1) + nat(3) .. nat(0)  ty_unknown }
%%%        4 -> { enum ( a(ty_id[5]) b(ty_id[6]) c(ty_id[7]) )
%%%        5 -> { enum_field(0) of ty_id[4] }
%%%        6 -> { enum_field(1) of ty_id[4] }
%%%        7 -> { enum_field(2) of ty_id[4] }
%%%        8 -> { alias to ty_id[4] }
%%%        9 -> { range ref(id:4) . c .. ref(id:4) . a  ty_unknown }
%%%       10 -> { ref_expr ref(id:4) }
%%%       11 -> { ref_expr ref(id:4) }
%%%       12 -> { range ref(id:6) .. ref(id:7)  ty_unknown }
%%% ]

%%% expected stderr:
%%% error line:2 column:9-35 Range bounds creates a negative-sized range
%%% error line:3 column:9-51 Range bounds creates a negative-sized range
%%% error line:4 column:9-19 Range bounds creates a negative-sized range
%%% error line:5 column:9-18 Range bounds creates a negative-sized range
%%% error line:10 column:13-23 Range bounds creates a negative-sized range
%%% error line:14 column:13-21 Range bounds creates a negative-sized range
