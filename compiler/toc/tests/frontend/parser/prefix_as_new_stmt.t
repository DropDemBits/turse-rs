% Identifiers and ^ are okay as they are interpreted as a new statement
const ba := 2
const a := 1 ba

const ba := 2
const a := 1 ^ba

%%% args: --only_parser -dump ast -b
%%% expected exit status: 0

%%% expected stdout:
%%% ast: [
%%% const [id:0] := nat(2)
%%% const [id:1] := nat(1)
%%% ref(id:0)()
%%% const [id:2] := nat(2)
%%% const [id:3] := nat(1)
%%% ^ref(id:2)()
%%% ]

%%% expected stderr:
