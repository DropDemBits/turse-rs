% Invalid terminators
if true then end
begin end % sync
if true then endcase
if true then

%%% args: --only_parser -dump ast -b
%%% expected exit status: 255

%%% expected stdout:
%%% ast: [
%%% if (bool(true)) then {}
%%% {}
%%% if (bool(true)) then {
%%%     if (bool(true)) then {}
%%% }
%%% ]

%%% expected stderr:
%%% error line:2 column:14-17 Missing 'if' after 'end' to finish statement
%%% error line:4 column:14-21 'endcase' does not begin a statement or declaration
%%% error line:25 column:1-1 Expected 'end' at the end of the if statement
%%% error line:25 column:1-1 Expected 'end' at the end of the if statement