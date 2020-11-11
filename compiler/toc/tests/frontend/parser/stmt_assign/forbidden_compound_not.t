% The forbidden not expression is invalid in `toc`
var a : boolean := true

a ~==~ a
begin end % synchronize point

a not==not a
begin end % synchronize point

%%% args: --only_parser -dump ast -b
%%% expected exit status: 255

%%% expected stdout:
%%% ast: {
%%%     var [id:0] : { prim Boolean } := bool(true)
%%%     ref(id:0)()
%%%     {}
%%%     ref(id:0)()
%%%     {}
%%% }

%%% expected stderr:
%%% error line:4 column:3-5 '~=' does not begin a statement or declaration
%%% error line:7 column:3-7 'not=' does not begin a statement or declaration