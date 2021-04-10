% Bare elsif is not okay, but should parse as an if
elsif true then end if
elif true then end if
elseif true then end if

%%% args: --only_parser -dump ast -b
%%% expected exit status: 255

%%% expected stdout:
%%% ast: {
%%%     if (bool(true)) then {}
%%%     if (bool(true)) then {}
%%%     if (bool(true)) then {}
%%% }

%%% expected stderr:
%%% error line:2 column:1-6 'elsif' without matching 'if'
%%% error line:3 column:1-5 'elif' without matching 'if'
%%% error line:4 column:1-7 'elseif' without matching 'if'
%%% warn line:3 column:6-10 'elif' found, assumed it to be 'elsif'
%%% warn line:4 column:8-12 'elseif' found, assumed it to be 'elsif'