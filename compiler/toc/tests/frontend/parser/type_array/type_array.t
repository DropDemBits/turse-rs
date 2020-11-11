% Array parsing setup
var start_range := 1
var end_range := 5

% Array parsing
var t : array 1 .. 2 of int
% Multiple ranges
var u : array 1 .. 2, (-1 - 20) .. (2 + 3), (1 + 8) .. (2 + 16) of string
% Char ranges
var v : array 'a' .. 'f' of real
var w : array char of nat
% Boolean ranges
var x : array false .. true of char
var y : array boolean of boolean
% Other ranges
var z : array start_range .. end_range of real
var implicit_size : array 1 .. * of real := init (1, 2, 3, 4, 5)
var flexi : flexible array 1 .. 0 of real

var up_size := 5
var runtime_size : array 1 .. up_size of real

%%% args: --only_parser -dump ast -b
%%% expected exit status: 0

%%% expected stdout:
%%% ast: [
%%% var [id:0] := nat(1)
%%% var [id:1] := nat(5)
%%% var [id:2] : { array { range nat(1) .. nat(2) } of { prim Int } }
%%% var [id:3] : { array { range nat(1) .. nat(2) }, { range (-nat(1) - nat(20)) .. (nat(2) + nat(3)) }, { range (nat(1) + nat(8)) .. (nat(2) + nat(16)) } of { prim String_ } }
%%% var [id:4] : { array { range 'a' .. 'f' } of { prim Real } }
%%% var [id:5] : { array { prim Char } of { prim Nat } }
%%% var [id:6] : { array { range bool(false) .. bool(true) } of { prim Char } }
%%% var [id:7] : { array { prim Boolean } of { prim Boolean } }
%%% var [id:8] : { array { range ref(id:0) .. ref(id:1) } of { prim Real } }
%%% var [id:9] : { array { range nat(1) .. * } of { prim Real } } := init(nat(1), nat(2), nat(3), nat(4), nat(5))
%%% var [id:10] : { flexible array { range nat(1) .. nat(0) } of { prim Real } }
%%% var [id:11] := nat(5)
%%% var [id:12] : { array { range nat(1) .. ref(id:11) } of { prim Real } }
%%% ]

%%% expected stderr:
