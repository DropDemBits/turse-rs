% Invalid forms - can't deduce type
var a
var c
var e, b, k

%%% args: --only_parser -dump ast -b
%%% expected exit status: 255

%%% expected stdout:
%%% ast: {
%%%     var [id:0] : { error }
%%%     var [id:1] : { error }
%%%     var [id:2, id:3, id:4] : { error }
%%% }

%%% expected stderr:
%%% error line:2 column:1-4 Cannot infer type for given var declaration (no type specification or initial value given)
%%% error line:3 column:1-4 Cannot infer type for given var declaration (no type specification or initial value given)
%%% error line:4 column:1-4 Cannot infer type for given var declaration (no type specification or initial value given)