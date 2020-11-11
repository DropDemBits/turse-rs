% Range bounds must be compile-time expressions
var a : int

type e : set of a..0
type f : set of 0..a
type g : set of a..a

%%% args: -M -dump types -b
%%% expected exit status: 255

%%% expected stdout:
%%% types: [
%%%        0 -> { alias to ty_id[3] }
%%%        1 -> { alias to ty_id[4] }
%%%        2 -> { alias to ty_id[5] }
%%%        3 -> { set of ty_error }
%%%        4 -> { set of ty_error }
%%%        5 -> { set of ty_error }
%%% ]

%%% expected stderr:
%%% error line:4 column:17-18 Start bound must be a compile-time expression
%%% error line:5 column:20-21 End bound must be a compile-time expression
%%% error line:6 column:17-18 Start bound must be a compile-time expression