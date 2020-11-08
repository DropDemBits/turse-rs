% Start overflows constitute an invalid range
var a : 16#ffffffffffffffff .. -3
var b : 16#ffffffffffffffff .. 16#7fffffffffffffff

%%% args: -M -dump types -b
%%% expected exit status: 255

%%% expected stdout:
%%% types: [
%%%        0 -> { range nat(18446744073709551615) .. -nat(3)  ty_unknown }
%%%        1 -> { range nat(18446744073709551615) .. nat(9223372036854775807)  ty_unknown }
%%% ]

%%% expected stderr:
%%% error line:2 column:9-34 Range bounds creates a negative-sized range
%%% error line:3 column:9-51 Range bounds creates a negative-sized range