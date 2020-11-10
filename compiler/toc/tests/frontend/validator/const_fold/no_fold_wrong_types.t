% Stop folding in the case of wrong types
var a : int := 1 - 1 - "bad" - 1 - 1
var b : int := 1 - 1 ** (0 - 1) - 1 - 1
var c : real := 10.0 ** (300 + 7) * 100
var d : real := 10.0 ** (300 + 10)
var e := false=0=-#-3
var f := -#-3
var g := --#-3

%%% args: -M -dump ast -b
%%% expected exit status: 255

%%% expected stdout:
%%% ast: [
%%% var [id:0] : { prim Int } := nat(1) - nat(1) - "bad" - nat(1) - nat(1)
%%% var [id:1] : { prim Int } := nat(1) - nat(1) ** (nat(0) - nat(1)) - nat(1) - nat(1)
%%% var [id:2] : { prim Real } := real(10) ** (nat(300) + nat(7)) * nat(100)
%%% var [id:3] : { prim Real } := real(10) ** (nat(300) + nat(10))
%%% var [id:4] := bool(false) = nat(0) = -#-nat(3)
%%% var [id:5] := -#-nat(3)
%%% var [id:6] := --#-nat(3)
%%% ]

%%% expected stderr:
%%% error line:2 column:22-23 Operands of '-' must both be scalars (int, real, or nat), or compatible sets
%%% error line:3 column:25-32 Invalid operand in compile-time expression
%%% error line:4 column:35-36 Overflow in compile-time expression
%%% error line:5 column:22-24 Overflow in compile-time expression
%%% error line:6 column:15-16 Operands of '=' must be the same type
%%% error line:6 column:17-18 Operands of '=' must be the same type
%%% error line:7 column:11-14 Overflow in compile-time expression
%%% error line:8 column:12-15 Overflow in compile-time expression