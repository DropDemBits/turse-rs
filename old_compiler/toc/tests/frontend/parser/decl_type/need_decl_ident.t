% Requires identifer, will consume the type and colon
type : a
type : int
type : 1 .. a

%%% args: --only_parser -dump ast -dump scope -dump types -b
%%% expected exit status: 255

%%% expected stdout:
%%% ast: {
%%%     type [] : { ref_expr ref(id:0) }
%%%     type [] : { prim Int }
%%%     type [] : { range nat(1) .. ref(id:0) }
%%% }
%%% scope: [
%%%        0 -> { a ty: ty_error, used: 2, var }
%%% ]
%%% types: [
%%% ]

%%% expected stderr:
%%% error line:2 column:6-7 Expected identifier after 'type'
%%% error line:3 column:6-7 Expected identifier after 'type'
%%% error line:4 column:6-7 Expected identifier after 'type'