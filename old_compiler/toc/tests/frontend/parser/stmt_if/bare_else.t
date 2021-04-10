% Bare else is not okay, but should parse as a block
else end if

%%% args: --only_parser -dump ast -b
%%% expected exit status: 255

%%% expected stdout:
%%% ast: {
%%%     {}
%%% }

%%% expected stderr:
%%% error line:2 column:1-5 'else' without matching 'if'