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
%%%        0 -> { alias to ty_id[1] }
%%%        1 -> { enum ( a(ty_id[2]) b(ty_id[3]) c(ty_id[4]) ) }
%%%        2 -> { enum_field(0) of ty_id[1] }
%%%        3 -> { enum_field(1) of ty_id[1] }
%%%        4 -> { enum_field(2) of ty_id[1] }
%%% ]

%%% expected stderr:
%%% error line:2 column:9-35 Range bounds creates a negative-sized range
%%% error line:3 column:9-51 Range bounds creates a negative-sized range
%%% error line:4 column:9-19 Range bounds creates a negative-sized range
%%% error line:5 column:9-18 Range bounds creates a negative-sized range
%%% error line:10 column:13-23 Range bounds creates a negative-sized range
%%% error line:14 column:13-21 Range bounds creates a negative-sized range
