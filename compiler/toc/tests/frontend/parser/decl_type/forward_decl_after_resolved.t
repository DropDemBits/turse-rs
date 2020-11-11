% Forward refs after resolves create a new type
type a : forward
type a : int
type a : forward

%%% args: --only_parser -dump ast -dump scope -dump types -b
%%% expected exit status: 0

%%% expected stdout:
%%% ast: {
%%%     type [id:0] : { forward }
%%%     type [id:0] : { prim Int }
%%%     type [id:1] : { forward }
%%% }
%%% scope: [
%%%        0 -> { a ty: ty_id[0], used: 0, tydef decl }
%%%        1 -> { a ty: ty_id[1], used: 0, tydef decl }
%%% ]
%%% types: [
%%%        0 -> { resolved forward }
%%%        1 -> { resolved forward }
%%% ]

%%% expected stderr:
