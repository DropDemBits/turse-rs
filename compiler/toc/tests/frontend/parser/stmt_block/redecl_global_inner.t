% Redeclaration of declared - global - inner
var yay : string := "hello!"

begin
    var yay : int := 5
end

% Should use the first declaration
yay

%%% args: --only_parser -dump ast -b
%%% expected exit status: 0

%%% expected stdout:
%%% ast: {
%%%     var [id:0] : { prim String_ } := "hello!"
%%%     {
%%%         var [id:1] : { prim Int } := nat(5)
%%%     }
%%%     ref(id:0)()
%%% }

%%% expected stderr:
