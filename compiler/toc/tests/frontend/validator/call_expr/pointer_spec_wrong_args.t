type typtr_int : ^int

var p_int : ^int

% Only 1 argument is allowed for pointer specialization
typtr_int(p_int, p_int, p_int)
typtr_int(p_int, p_int)
typtr_int()

% Reference must be a const/var reference
typtr_int(typtr_int)

%%% args: -b
%%% expected exit status: 255

%%% expected stderr:
%%% error line:6 column:25-30 Too many arguments for pointer specialization (expected 1, found 3)
%%% error line:6 column:1-10 Pointer specialization expressions are not supported yet
%%% error line:7 column:18-23 Too many arguments for pointer specialization (expected 1, found 2)
%%% error line:7 column:1-10 Pointer specialization expressions are not supported yet
%%% error line:8 column:10-11 Pointer specialization requires 1 argument
%%% error line:8 column:1-10 Pointer specialization expressions are not supported yet
%%% error line:11 column:11-20 Expression refers to a type, and is not allowed here
%%% error line:11 column:1-10 Pointer specialization expressions are not supported yet