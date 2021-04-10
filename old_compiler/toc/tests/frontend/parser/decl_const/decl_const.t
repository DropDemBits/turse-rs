% Valid forms
const a : int := 1
const b := 5.0
const c, d : int := 3
const e, f := 3 + 6 ** 2

const * a : int := 1
const pervasive a : int := 1

begin
    % Register declarations can only be contained in deeper scopes
    const register a := 1
    const pervasive register a := 1
end

% Accepted forms
const g : int = -5
const h : int = -10 + 3 * 2

%%% args: --only_parser -dump ast -dump scope -b
%%% expected exit status: 0

%%% expected stdout:
%%% ast: {
%%%     const [id:0] : { prim Int } := nat(1)
%%%     const [id:1] := real(5)
%%%     const [id:2, id:3] : { prim Int } := nat(3)
%%%     const [id:4, id:5] := nat(3) + nat(6) ** nat(2)
%%%     const [id:6] : { prim Int } := nat(1)
%%%     const [id:7] : { prim Int } := nat(1)
%%%     {
%%%         const register [id:8] := nat(1)
%%%         const register [id:9] := nat(1)
%%%     }
%%%     const [id:10] : { prim Int } := -nat(5)
%%%     const [id:11] : { prim Int } := -nat(10) + nat(3) * nat(2)
%%% }
%%% scope: [
%%%        0 -> { a ty: ty_unknown, used: 0, const decl }
%%%        1 -> { b ty: ty_unknown, used: 0, const decl }
%%%        2 -> { c ty: ty_unknown, used: 0, const decl }
%%%        3 -> { d ty: ty_unknown, used: 0, const decl }
%%%        4 -> { e ty: ty_unknown, used: 0, const decl }
%%%        5 -> { f ty: ty_unknown, used: 0, const decl }
%%%        6 -> { a ty: ty_unknown, used: 0, const decl pervasive }
%%%        7 -> { a ty: ty_unknown, used: 0, const decl pervasive }
%%%        8 -> { a ty: ty_unknown, used: 0, const decl }
%%%        9 -> { a ty: ty_unknown, used: 0, const decl pervasive }
%%%       10 -> { g ty: ty_unknown, used: 0, const decl }
%%%       11 -> { h ty: ty_unknown, used: 0, const decl }
%%% ]

%%% expected stderr:
%%% warn line:17 column:15-16 '=' found, assumed it to be ':='
%%% warn line:18 column:15-16 '=' found, assumed it to be ':='