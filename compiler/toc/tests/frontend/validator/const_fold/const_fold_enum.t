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
%%% ast: {
%%%     type [id:0] : { enum ( a, b, c, ) }
%%%     {
%%%         var [id:1] := bool(true)
%%%     }
%%%     {
%%%         const [id:2] : { ref_expr ref(id:0) } := nat(2)
%%%         var [id:3] := bool(true)
%%%     }
%%%     {
%%%         const [id:4] := nat(2)
%%%         var [id:5] := bool(true)
%%%     }
%%% }

%%% expected stderr:
