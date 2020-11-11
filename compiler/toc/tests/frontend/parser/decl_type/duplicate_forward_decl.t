% Duplicate forward refs should not affect resolved state (and should share IdentId)
type a : forward
type a : forward
type a : forward
type a : forward
type a : forward
type a : int

%%% args: --only_parser -dump ast -dump scope -dump types -b
%%% expected exit status: 255

%%% expected stdout:
%%% ast: [
%%% type [id:0] : { forward }
%%% type [id:0] : { forward }
%%% type [id:0] : { forward }
%%% type [id:0] : { forward }
%%% type [id:0] : { forward }
%%% type [id:0] : { prim Int }
%%% ]
%%% scope: [
%%%        0 -> { a ty: ty_id[0], used: 0, tydef decl }
%%% ]
%%% types: [
%%%        0 -> { resolved forward }
%%% ]

%%% expected stderr:
%%% error line:3 column:6-7 Duplicate forward type declaration
%%% error line:4 column:6-7 Duplicate forward type declaration
%%% error line:5 column:6-7 Duplicate forward type declaration
%%% error line:6 column:6-7 Duplicate forward type declaration