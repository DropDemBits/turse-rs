% Multiple import statments aren't supported (and are only allowed inside top-level declarations)
import "a", b, c
import d, e, f in "g"

begin
    % Not allowed in here
    import local_here
end

local_here := 1

%%% args: --only_parser -dump ast -dump scope -b

%%% expected stdout:
%%% ast: {
%%%     import ( in "a", [id:0] , [id:1]  )
%%%     import ( [id:2] , [id:3] , [id:4] in "g" )
%%%     {
%%%         import ( [id:5]  )
%%%     }
%%%     ref(id:6) := nat(1)
%%% }
%%% scope: [
%%%        0 -> { b ty: ty_unknown, used: 0, const decl }
%%%        1 -> { c ty: ty_unknown, used: 0, const decl }
%%%        2 -> { d ty: ty_unknown, used: 0, const decl }
%%%        3 -> { e ty: ty_unknown, used: 0, const decl }
%%%        4 -> { f ty: ty_unknown, used: 0, const decl }
%%%        5 -> { local_here ty: ty_unknown, used: 0, const decl }
%%%        6 -> { local_here ty: ty_error, used: 1, var }
%%% ]

%%% expected stderr:
%%% error line:3 column:1-7 Import statements are not allowed here
%%% error line:7 column:5-11 Import statements are not allowed here