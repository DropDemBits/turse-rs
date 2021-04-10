% Redeclaration of declared - inner - inner
begin
    var yay : int := 5
    yay % Use above decl

    begin
        var yay : int := 5
        yay % Use above decl
    end

    yay % Use previous decl
end

yay % Completely never before seen, should use a new decl

var yay : string := "hello!"
yay % Use above decl

%%% args: --only_parser -dump ast -b
%%% expected exit status: 0

%%% expected stdout:
%%% ast: {
%%%     {
%%%         var [id:0] : { prim Int } := nat(5)
%%%         ref(id:0)()
%%%         {
%%%             var [id:1] : { prim Int } := nat(5)
%%%             ref(id:1)()
%%%         }
%%%         ref(id:0)()
%%%     }
%%%     ref(id:2)()
%%%     var [id:3] : { prim String_ } := "hello!"
%%%     ref(id:3)()
%%% }

%%% expected stderr:
