a += 1
begin
    var a : int := 1
end
a += 1 % should reference id(0)

%%% args: -M -dump ast -dump scope -b
%%% expected exit status: 255

%%% expected stdout:
%%% ast: [
%%% ref(id:0) += nat(1)
%%% {
%%%     var [id:1] : { prim Int } := nat(1)
%%% }
%%% ref(id:0) += nat(1)
%%% ]
%%% scope: [
%%%        0 -> { a ty: ty_error, used: 2, var }
%%%        1 -> { a ty: ty_prim[Int], used: 0, var decl }
%%% ]

%%% expected stderr:
%%% error line:1 column:1-2 'a' has not been declared yet
%%% error line:3 column:9-10 'a' has already been declared
%%% error line:5 column:1-2 'a' has not been declared yet