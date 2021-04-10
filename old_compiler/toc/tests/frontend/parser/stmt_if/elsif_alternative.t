% Warn on alternative `elsif`s
if true then
elseif true then
elif true then
end if

%%% args: --only_parser -dump ast -b
%%% expected exit status: 0

%%% expected stdout:
%%% ast: {
%%%     if (bool(true)) then {}
%%%     else if (bool(true)) then {}
%%%     else if (bool(true)) then {}
%%% }

%%% expected stderr:
%%% warn line:3 column:8-12 'elseif' found, assumed it to be 'elsif'
%%% warn line:4 column:6-10 'elif' found, assumed it to be 'elsif'