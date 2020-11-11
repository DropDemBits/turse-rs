type a : int
type a : array 1 .. 2 of int

type c : forward
type c : int

%%% args: --only_parser -dump ast -dump scope -dump types -b
%%% expected exit status: 0

%%% expected stdout:
%%% ast: [
%%% type [id:0] : { prim Int }
%%% type [id:1] : { array { range nat(1) .. nat(2) } of { prim Int } }
%%% type [id:2] : { forward }
%%% type [id:2] : { prim Int }
%%% ]
%%% scope: [
%%%        0 -> { a ty: ty_id[0], used: 0, tydef decl }
%%%        1 -> { a ty: ty_id[1], used: 0, tydef decl }
%%%        2 -> { c ty: ty_id[2], used: 0, tydef decl }
%%% ]
%%% types: [
%%%        0 -> { resolved forward }
%%%        1 -> { resolved forward }
%%%        2 -> { resolved forward }
%%% ]

%%% expected stderr:
