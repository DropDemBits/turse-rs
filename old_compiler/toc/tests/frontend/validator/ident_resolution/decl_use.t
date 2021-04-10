var a : int := 1
a += 1

%%% args: -M -dump scope -b
%%% expected exit status: 0

%%% expected stdout:
%%% scope: [
%%%        0 -> { a ty: ty_prim[Int], used: 1, var decl }
%%% ]

%%% expected stderr: