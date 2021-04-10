% Even though positive literals are NatValues, the transferred type should still
% allow implicit type variables to be assigned negative integers
var a := 1 + 1
a := -1

var b := 1
b := -1

%%% args: -M -dump scope -b
%%% expected exit status: 0

%%% expected stdout:
%%% scope: [
%%%        0 -> { a ty: ty_prim[Int], used: 1, var decl }
%%%        1 -> { b ty: ty_prim[Int], used: 1, var decl }
%%% ]

%%% expected stderr:
