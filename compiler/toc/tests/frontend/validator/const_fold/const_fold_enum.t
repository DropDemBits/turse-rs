% Folding should be able to fold enum comparisons
type e0 : enum (a, b, c)

% Direct enum field comparison
begin
    var a := e0.a < e0.c
end

% Known enum type
begin
    const c : e0 := e0.c
    var a := e0.a < c
end

% Propogate field
begin
    const c := e0.c
    var a := e0.a < c
end

%%% args: -M -dump ast -b
%%% expected exit status: 0

%%% expected stdout:
%%% ast: [
%%% type [id:0] : ty_id[4]
%%% {
%%%     var [id:1] : ty_prim[Boolean] := bool(true)
%%% }
%%% {
%%%     const [id:2] : ty_id[0] := nat(2)
%%%     var [id:3] : ty_prim[Boolean] := bool(true)
%%% }
%%% {
%%%     const [id:4] : ty_id[3] := nat(2)
%%%     var [id:5] : ty_prim[Boolean] := bool(true)
%%% }
%%% ]

%%% expected stderr:
