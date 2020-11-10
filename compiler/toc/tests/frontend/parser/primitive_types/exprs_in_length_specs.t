% Expressions are allowed for string(n) and char(n), resolved at validator time
% They don't parse into to the base type
% Wrong types will be captured by the validator
var c : string(1 + 1 + 1 - 2 + 4 * 8 div 2)

const c := 1 + 1 + 1 - 2 + 4 * 8 div 2
var d : string(c)

const c := 1 + 1 + 1 - 2 + 4 * 8 div 2
var d : char(c + 4)

%%% args: --only_parser -dump ast -b
%%% expected exit status: 0

%%% expected stdout:
%%% ast: [
%%% var [id:0] : { string(nat(1) + nat(1) + nat(1) - nat(2) + nat(4) * nat(8) div nat(2)) }
%%% const [id:1] := nat(1) + nat(1) + nat(1) - nat(2) + nat(4) * nat(8) div nat(2)
%%% var [id:2] : { string(ref(id:1)) }
%%% const [id:3] := nat(1) + nat(1) + nat(1) - nat(2) + nat(4) * nat(8) div nat(2)
%%% var [id:4] : { char(ref(id:3) + nat(4)) }
%%% ]

%%% expected stderr:
