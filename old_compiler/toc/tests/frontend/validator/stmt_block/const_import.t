% Importing consts should work
const outer_see : int := 5

begin
    const middle_see : int := 262

    begin
        var inner_see : outer_see .. middle_see + outer_see
    end

    begin
        var and_here_too : int := middle_see + outer_see
    end
end

%%% args: -dump ast -dump types -M -b
%%% expected exit status: 0

%%% expected stdout:
%%% ast: {
%%%     const [id:0] : { prim Int } := nat(5)
%%%     {
%%%         const [id:1] : { prim Int } := nat(262)
%%%         {
%%%             var [id:2] : { range nat(5) .. nat(267) }
%%%         }
%%%         {
%%%             var [id:3] : { prim Int } := nat(267)
%%%         }
%%%     }
%%% }
%%% types: [
%%%        0 -> { range 5 .. 267 (263) ty_prim[Int] }
%%% ]
 
%%% expected stderr:
