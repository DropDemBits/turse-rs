% Invalid forms - No value
const a : int
const b : int
const c, d, e : int

%%% args: --only_parser -dump ast -b
%%% expected exit status: 255

%%% expected stdout:
%%% ast: {
%%%     const [id:0] : { error }
%%%     const [id:1] : { error }
%%%     const [id:2, id:3, id:4] : { error }
%%% }

%%% expected stderr:
%%% error line:2 column:1-6 const declaration requires an initial value
%%% error line:3 column:1-6 const declaration requires an initial value
%%% error line:4 column:1-6 const declaration requires an initial value