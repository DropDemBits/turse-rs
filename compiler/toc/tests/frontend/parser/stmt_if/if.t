if true then
    var a := 1
end if

%%% args: --only_parser -dump ast -b
%%% expected exit status: 0

%%% expected stdout:
%%% ast: {
%%%     if (bool(true)) then {
%%%         var [id:0] := nat(1)
%%%     }
%%% }

%%% expected stderr:
