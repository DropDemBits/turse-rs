% Accept `endif` as terminator
if true then endif

%%% args: --only_parser -dump ast -b
%%% expected exit status: 0

%%% expected stdout:
%%% ast: {
%%%     if (bool(true)) then {}
%%% }

%%% expected stderr:
%%% warn line:2 column:14-19 'endif' found, assumed it to be 'end if'