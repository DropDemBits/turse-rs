% Report error about missing end after if
if false then end
% Shouldn't skip over consecutive if statement
if false then end if

if false then end
begin end

%%% args: --only_parser -dump ast -b
%%% expected exit status: 255

%%% expected stdout:
%%% ast: [
%%% if (bool(false)) then {}
%%% if (bool(false)) then {}
%%% if (bool(false)) then {}
%%% {}
%%% ]

%%% expected stderr:
%%% error line:2 column:15-18 Missing 'if' after 'end' to finish statement
%%% error line:6 column:15-18 Missing 'if' after 'end' to finish statement