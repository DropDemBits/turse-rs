% Optional parentheses
import ( "a", b )

%%% args: --only_parser -dump ast -dump scope -b
%%% expected exit status: 0

%%% expected stdout:
%%% ast: {
%%%     import ( in "a", [id:0]  )
%%% }
%%% scope: [
%%%        0 -> { b ty: ty_unknown, used: 0, const decl }
%%% ]

%%% expected stderr:
