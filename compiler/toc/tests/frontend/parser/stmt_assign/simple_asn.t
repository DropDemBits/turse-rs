% Setup
var a : int

% Valid forms
a := 1
a := 3 + 5 + 7
a := #9 * 2 ** 3 and 5 xor 6

% Accepted forms
a = 2
a = 194812
a = -6

%%% args: --only_parser -dump ast -b
%%% expected exit status: 0

%%% expected stdout:
%%% ast: {
%%%     var [id:0] : { prim Int }
%%%     ref(id:0) := nat(1)
%%%     ref(id:0) := nat(3) + nat(5) + nat(7)
%%%     ref(id:0) := #nat(9) * nat(2) ** nat(3) and nat(5) xor nat(6)
%%%     ref(id:0) := nat(2)
%%%     ref(id:0) := nat(194812)
%%%     ref(id:0) := -nat(6)
%%% }

%%% expected stderr:
%%% warn line:10 column:3-4 '=' found, assumed it to be ':='
%%% warn line:11 column:3-4 '=' found, assumed it to be ':='
%%% warn line:12 column:3-4 '=' found, assumed it to be ':='
