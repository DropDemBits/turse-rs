% Range parsing
var a_range : (1 - 3 shl 5) .. (2 * 50 - 8 * 4)

% Zero-sized and negative size ranges are only checked in Validator
type other_ranges : 'c' .. 'c'
type other_ranges : 'c' .. 'f'
type other_ranges : 'f' .. 'c'
type other_ranges : false .. false
type other_ranges : true .. false
type other_ranges : false .. true
type other_ranges : a .. b

%%% args: --only_parser -dump ast -b
%%% expected exit status: 0

%%% expected stdout:
%%% ast: {
%%%     var [id:0] : { range (nat(1) - nat(3) shl nat(5)) .. (nat(2) * nat(50) - nat(8) * nat(4)) }
%%%     type [id:1] : { range 'c' .. 'c' }
%%%     type [id:2] : { range 'c' .. 'f' }
%%%     type [id:3] : { range 'f' .. 'c' }
%%%     type [id:4] : { range bool(false) .. bool(false) }
%%%     type [id:5] : { range bool(true) .. bool(false) }
%%%     type [id:6] : { range bool(false) .. bool(true) }
%%%     type [id:9] : { range ref(id:7) .. ref(id:8) }
%%% }

%%% expected stderr:
