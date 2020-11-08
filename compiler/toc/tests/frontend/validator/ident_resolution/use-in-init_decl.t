var a : string := a + "oops"
a := "ok" % final type

%%% args: -M -dump scope -b
%%% expected exit status: 255

%%% expected stdout:
%%% scope: [
%%%        0 -> { a ty: ty_error, used: 1, var }
%%%        1 -> { a ty: ty_prim[String_], used: 1, var decl }
%%% ]

%%% expected stderr:
%%% error line:1 column:5-6 'a' has already been declared
%%% error line:1 column:19-20 'a' has not been declared yet