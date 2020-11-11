% Requires colon, will parse the rest and produce a declaration
var a : string
type a      begin end
type a int

%%% args: --only_parser -dump ast -dump scope -dump types -b
%%% expected exit status: 255

%%% expected stdout:
%%% ast: [
%%% var [id:0] : { prim String_ }
%%% type [id:1] : { error }
%%% {}
%%% type [id:2] : { prim Int }
%%% ]
%%% scope: [
%%%        0 -> { a ty: ty_unknown, used: 0, var decl }
%%%        1 -> { a ty: ty_id[0], used: 0, tydef decl }
%%%        2 -> { a ty: ty_id[1], used: 0, tydef decl }
%%% ]
%%% types: [
%%%        0 -> { resolved forward }
%%%        1 -> { resolved forward }
%%% ]

%%% expected stderr:
%%% error line:3 column:13-18 Expected ':' after identifier
%%% error line:3 column:13-18 Expected expression before 'begin' 
%%% error line:3 column:13-18 Unexpected 'begin', expected a type specifier
%%% error line:4 column:8-11 Expected ':' after identifier