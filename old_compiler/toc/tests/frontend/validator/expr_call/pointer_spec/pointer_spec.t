% Not fully typeck'd yet, except for arg count checks & checking if arg is a const/var ref
% TODO: Check full specialization once collection types are added

type typtr_int : ^int

var p_int : ^int

% Pointer specialization isn't fully checked yet
typtr_int(p_int)

%%% args: -b
%%% expected exit status: 255

%%% expected stderr:
%%% error line:9 column:1-10 Pointer specialization expressions are not supported yet