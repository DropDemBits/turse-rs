var keke := 2

if true then
    keke := 3
end if

%%% args: --only_parser -dump ast -b
%%% expected exit status: 0

%%% expected stdout:
%%% ast: {
%%%     var [id:0] := nat(2)
%%%     if (bool(true)) then {
%%%         ref(id:0) := nat(3)
%%%     }
%%% }

%%% expected stderr:
