var a : string
var a : real8
a := 1.0 % final type

%%% args: -M -dump scope -b
%%% expected exit status: 255

%%% expected stdout:
%%% scope: [
%%%        0 -> { a ty: ty_prim[String_], used: 0, var decl }
%%%        1 -> { a ty: ty_prim[Real8], used: 1, var decl }
%%% ]

%%% expected stderr:
%%% error line:2 column:5-6 'a' has already been declared