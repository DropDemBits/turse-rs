% Don't fold binary & unary expressions in the event of an error
const a := 4
const b := a + 1        % (4 + 1)
const c := b + 1 + a + "beep beep"
const d := a + b + c    % 4*4 + 1 + 1 + 1

%%% args: -M -dump ast -b
%%% expected exit status: 255

%%% expected stdout:
%%% ast: [
%%% const [id:0] : ty_prim[Int] := nat(4)
%%% const [id:1] : ty_prim[Int] := nat(5)
%%% const [id:2] : ty_error := ref(id:1) + nat(1) + ref(id:0) + "beep beep"
%%% const [id:3] : ty_error := ref(id:0) + ref(id:1) + ref(id:2)
%%% ]

%%% expected stderr:
%%% error line:4 column:22-23 Operands of '+' must both be scalars (int, real, or nat), strings, or compatible sets
