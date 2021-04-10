% Set parsing (only valid in type statements)
type some_set : set of 1 .. 5
type some_set_c : set of char
type some_set_b : set of boolean

%%% args: --only_parser -dump ast -b
%%% expected exit status: 0

%%% expected stdout:
%%% ast: {
%%%     type [id:0] : { set of { range nat(1) .. nat(5) } }
%%%     type [id:1] : { set of { prim Char } }
%%%     type [id:2] : { set of { prim Boolean } }
%%% }

%%% expected stderr:
