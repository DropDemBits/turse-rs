% Arbitrary expressions are not valid types
var a : 1
var a : 1 ** 2
var a : (1 * 6 - 1 + 4 = 1)
var a : false

%%% args: --only_parser -dump ast -b
%%% expected exit status: 255

%%% expected stdout:
%%% ast: [
%%% var [id:0] : { ref_expr nat(1) }
%%% var [id:1] : { ref_expr nat(1) ** nat(2) }
%%% var [id:2] : { ref_expr (nat(1) * nat(6) - nat(1) + nat(4) = nat(1)) }
%%% var [id:3] : { ref_expr bool(false) }
%%% ]

%%% expected stderr:
%%% error line:2 column:9-10 Expression is not a valid type reference
%%% error line:3 column:9-15 Expression is not a valid type reference
%%% error line:4 column:9-28 Expression is not a valid type reference
%%% error line:5 column:9-14 Expression is not a valid type reference
