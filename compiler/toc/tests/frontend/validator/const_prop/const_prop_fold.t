% Fold constants together
const a := 4
const b := a + 1        % (4 + 1)
const c := b + 1 + a    % (4 + 1) + 1 + 4
const d := a + b + c    % 4*4 + 1 + 1 + 1

%%% args: -M -dump ast -b
%%% expected exit status: 0

%%% expected stdout:
%%% ast: [
%%% const [id:0] : ty_prim[Int] := nat(4)
%%% const [id:1] : ty_prim[Int] := nat(5)
%%% const [id:2] : ty_prim[Int] := nat(10)
%%% const [id:3] : ty_prim[Int] := nat(19)
%%% ]

%%% expected stderr:
