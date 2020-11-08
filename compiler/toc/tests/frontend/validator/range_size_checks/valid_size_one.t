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
%%%        0 -> { range "a" .. "a" (1) ty_prim[StringN(Size(1))] }
%%%        1 -> { range nat(1) .. nat(1) (1) ty_prim[Int] }
%%%        2 -> { range bool(true) .. bool(true) (1) ty_prim[Boolean] }
%%%        3 -> { range bool(false) .. bool(false) (1) ty_prim[Boolean] }
%%%        4 -> { enum ( a(ty_id[5]) b(ty_id[6]) )
%%%        5 -> { enum_field(0) of ty_id[4] }
%%%        6 -> { enum_field(1) of ty_id[4] }
%%%        7 -> { alias to ty_id[4] }
%%%        8 -> { range nat(0) .. nat(0) (1) ty_id[4] }
%%%        9 -> { ref_expr ref(id:4) }
%%%       10 -> { range nat(0) .. nat(0) (1) ty_id[4] }
%%%       11 -> { range nat(0) .. nat(0) (1) ty_prim[Int] }
%%% ]

%%% expected stderr:
