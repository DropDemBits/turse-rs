% Star lengths in subprogram parameters
var a : proc _ (a : string(*))
var b : proc _ (b : char(*))

%%% args: --only_parser -dump ast -b
%%% expected exit status: 0

%%% expected stdout:
%%% ast: {
%%%     var [id:0] : { procedure (a : { string(*) }) }
%%%     var [id:1] : { procedure (b : { char(*) }) }
%%% }

%%% expected stderr:
