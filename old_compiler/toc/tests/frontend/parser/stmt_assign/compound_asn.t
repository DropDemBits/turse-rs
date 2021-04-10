% Main operators
% Setup
var a : int
var r : real
var b : boolean

% Valid forms
a := 3
a += 5
a -= 7
a *= 9
a div= 11
r /= 12.0
a rem= 3
a mod= 5
a **= 2
a and= 3
a or= 5
a xor= 6
a shl= 9
a shr= 12

% Boolean operators
% Valid forms
b =>= true
b and= false
b or= true
% xor= only valid for integers (int, nat, long, ulong) & sets

%%% args: --only_parser -dump ast -b
%%% expected exit status: 0

%%% expected stdout:
%%% ast: {
%%%     var [id:0] : { prim Int }
%%%     var [id:1] : { prim Real }
%%%     var [id:2] : { prim Boolean }
%%%     ref(id:0) := nat(3)
%%%     ref(id:0) += nat(5)
%%%     ref(id:0) -= nat(7)
%%%     ref(id:0) *= nat(9)
%%%     ref(id:0) div= nat(11)
%%%     ref(id:1) /= real(12)
%%%     ref(id:0) rem= nat(3)
%%%     ref(id:0) mod= nat(5)
%%%     ref(id:0) **= nat(2)
%%%     ref(id:0) and= nat(3)
%%%     ref(id:0) or= nat(5)
%%%     ref(id:0) xor= nat(6)
%%%     ref(id:0) shl= nat(9)
%%%     ref(id:0) shr= nat(12)
%%%     ref(id:2) =>= bool(true)
%%%     ref(id:2) and= bool(false)
%%%     ref(id:2) or= bool(true)
%%% }

%%% expected stderr:
