% Sister blocks should share the same undeclared identifiers
begin
    a
end

begin
    a
end

if true then
    a
end if

a

%%% args: --only_parser -dump ast -dump scope -b
%%% expected exit code: 0

%%% expected stdout:
%%% ast: [
%%% {
%%%     ref(id:0)()
%%% }
%%% {
%%%     ref(id:0)()
%%% }
%%% if (bool(true)) then {
%%%     ref(id:0)()
%%% }
%%% ref(id:0)()
%%% ]
%%% scope: [
%%%        0 -> { a ty: ty_error, used: 4, var }
%%% ]

%%% expected stderr:
