a += 1

%%% args: -M -dump scope -b
%%% expected exit status: 255

%%% expected stdout:
%%% scope: [
%%%        0 -> { a ty: ty_error, used: 1, var }
%%% ]

%%% expected stderr:
%%% error line:1 column:1-2 'a' has not been declared yet