% Test that the correct inferred types are being used
var a := 16#7FFFFFFF
var b := 16#80000000

% These 4 should fail if implicitly interpreting as Int/Nat instead of LongInt / LongNat
% - 64-bit is opt-in, not the default
% - eval constraints should limit this into the 32-bit range for implicit operations
var c := 16#100000000
var d := 16#100000000 + 16#100000000
var e := 16#8000000000000000
var f := 16#8000000000000000 + 1

%%% args: -M -dump scope -b

%%% expected stdout:
%%% scope: [
%%%        0 -> { a ty: ty_prim[Int], used: 0, var decl }
%%%        1 -> { b ty: ty_prim[Nat], used: 0, var decl }
%%%        2 -> { c ty: ty_prim[LongInt], used: 0, var decl }
%%%        3 -> { d ty: ty_prim[LongInt], used: 0, var decl }
%%%        4 -> { e ty: ty_prim[LongNat], used: 0, var decl }
%%%        5 -> { f ty: ty_prim[LongNat], used: 0, var decl }
%%% ]