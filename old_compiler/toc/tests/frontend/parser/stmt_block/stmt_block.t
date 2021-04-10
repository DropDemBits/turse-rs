% Local declarations & importation
begin
    var hey := 2

    begin
        var yay : real := 5 + hey
    end

    begin
        % Different scope!
        var yay : real := 5 + hey
    end

    var yay : int := 6 - hey
end

var yay : string := "hello!"

%%% args: --only_parser -dump ast -b
%%% expected exit status: 0

%%% expected stdout:
%%% ast: {
%%%     {
%%%         var [id:0] := nat(2)
%%%         {
%%%             var [id:1] : { prim Real } := nat(5) + ref(id:0)
%%%         }
%%%         {
%%%             var [id:2] : { prim Real } := nat(5) + ref(id:0)
%%%         }
%%%         var [id:3] : { prim Int } := nat(6) - ref(id:0)
%%%     }
%%%     var [id:4] : { prim String_ } := "hello!"
%%% }

%%% expected stderr:
