if true then
    var a := 1
elsif false then
    var a := 2
end if

%%% args: --only_parser -dump ast -b
%%% expected exit status: 0

%%% expected stdout:
%%% ast: [
%%% if (bool(true)) then {
%%%     var [id:0] := nat(1)
%%% }
%%% else if (bool(false)) then {
%%%     var [id:1] := nat(2)
%%% }
%%% ]

%%% expected stderr:
