% 1 sized ranges are valid
begin
    var a : 'a' .. 'a'
    var b : 1 .. 1
    var c : true .. true
    var d : false .. false
end

begin
    type e : enum(a, b)

    var a : e.a .. e.a
    
    const c : e := e.a
    var b : c .. c
end

begin
    const s : int := 0
    const e : int := 0
    var a : s .. e
end

%%% args: -M -dump types -b
%%% expected exit status: 0

%%% expected stdout:
%%% types: [
%%%        0 -> { alias to ty_id[5] }
%%%        1 -> { range 97 .. 97 (1) ty_prim[StringN(Size(1))] }
%%%        2 -> { range 1 .. 1 (1) ty_prim[Int] }
%%%        3 -> { range 1 .. 1 (1) ty_prim[Boolean] }
%%%        4 -> { range 0 .. 0 (1) ty_prim[Boolean] }
%%%        5 -> { enum ( a(ty_id[6]) b(ty_id[7]) ) }
%%%        6 -> { enum_field(0) of ty_id[5] }
%%%        7 -> { enum_field(1) of ty_id[5] }
%%%        8 -> { range 0 .. 0 (1) ty_id[5] }
%%%        9 -> { range 0 .. 0 (1) ty_id[5] }
%%%       10 -> { range 0 .. 0 (1) ty_prim[Int] }
%%% ]

%%% expected stderr:
