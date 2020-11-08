% Range bounds must be compile-time expressions
var a : int

type e : set of a..0
type f : set of 0..a
type g : set of a..a

%%% args: -M -dump types -b
%%% expected exit status: 255

%%% expected stdout:
%%% types: [
%%%        0 -> { range ref(id:0) .. nat(0)  ty_unknown }
%%%        1 -> { set of ty_error }
%%%        2 -> { alias to ty_id[1] }
%%%        3 -> { range nat(0) .. ref(id:0)  ty_unknown }
%%%        4 -> { set of ty_error }
%%%        5 -> { alias to ty_id[4] }
%%%        6 -> { range ref(id:0) .. ref(id:0)  ty_unknown }
%%%        7 -> { set of ty_error }
%%%        8 -> { alias to ty_id[7] }
%%% ]

%%% expected stderr:
%%% error line:4 column:17-18 Start bound must be a compile-time expression
%%% error line:5 column:20-21 End bound must be a compile-time expression
%%% error line:6 column:17-18 Start bound must be a compile-time expression