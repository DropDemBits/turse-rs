% Check that the forward reference is updated
type unresolved : forward
type resolved : forward
type resolved : int

%%% args: --only_parser -dump ast -dump scope -dump types -b
%%% expected exit status: 0

%%% expected stdout:
%%% ast: {
%%%     type [id:0] : { forward }
%%%     type [id:1] : { forward }
%%%     type [id:1] : { prim Int }
%%% }
%%% scope: [
%%%        0 -> { unresolved ty: ty_id[0], used: 0, tydef decl }
%%%        1 -> { resolved ty: ty_id[1], used: 0, tydef decl }
%%% ]
%%% types: [
%%%        0 -> { forward }
%%%        1 -> { resolved forward }
%%% ]

%%% expected stderr:
