% Multiple undeclared identifier error should be produced
a += 1
a += 1
var b : int := 1

%%% args: -M -dump scope -b
%%% expected exit status: 255

%%% expected stdout:
%%% scope: [
%%%        0 -> { a ty: ty_error, used: 2, var }
%%%        1 -> { b ty: ty_prim[Int], used: 0, var decl }
%%% ]

%%% expected stderr:
%%% error line:2 column:1-2 'a' has not been declared yet
%%% error line:3 column:1-2 'a' has not been declared yet