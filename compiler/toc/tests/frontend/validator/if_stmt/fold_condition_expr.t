% Condition expression should be folded

if true & true then
    var k := 2
    k := k
end if

if false & false then
    var k := 2
    k := k
elsif true & true then
    var c := 3
    c := c
end if

%%% args: -dump ast -b
%%% expected exit status: 0

%%% expected stdout:
%%% ast: [
%%% if (bool(true)) then {
%%%     var [id:0] : ty_prim[Int] := nat(2)
%%%     ref(id:0) := ref(id:0)
%%% }
%%% if (bool(false)) then {
%%%     var [id:1] : ty_prim[Int] := nat(2)
%%%     ref(id:1) := ref(id:1)
%%% }
%%% else if (bool(true)) then {
%%%     var [id:2] : ty_prim[Int] := nat(3)
%%%     ref(id:2) := ref(id:2)
%%% }
%%% ]

%%% expected stderr:
