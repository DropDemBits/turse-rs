a := a + 1
var a : int
var a : string
a := "ok" % final type

%%% args: -M -dump scope -b
%%% expected exit status: 255

%%% expected stdout:
%%% scope: [
%%%        0 -> { a ty: ty_error, used: 2, var }
%%%        1 -> { a ty: ty_prim[Int], used: 0, var decl }
%%%        2 -> { a ty: ty_prim[String_], used: 1, var decl }
%%% ]

%%% expected stderr:
%%% error line:1 column:1-2 'a' has not been declared yet
%%% error line:1 column:6-7 'a' has not been declared yet
%%% error line:3 column:5-6 'a' has already been declared