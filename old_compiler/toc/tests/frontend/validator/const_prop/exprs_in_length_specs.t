% Constant folding allows compile-time expressions in length specifiers
const sz := 3

const a : string(sz) := 'aaa'
const b : char(sz + 10) := 'aaa'

%%% args: -M -dump scope -b
%%% expected exit status: 0

%%% expected stdout:
%%% scope: [
%%%        0 -> { sz ty: ty_prim[Int], used: 2, const decl comp_eval }
%%%        1 -> { a ty: ty_prim[StringN(Size(3))], used: 0, const decl comp_eval }
%%%        2 -> { b ty: ty_prim[CharN(Size(13))], used: 0, const decl comp_eval }
%%% ]

%%% expected stderr:
