% Basic primitive types
var a : boolean
var b : int
var c : int1
var d : int2
var e : int4
var f : nat
var g : nat1
var h : nat2
var i : nat4
var j : real
var k : real4
var l : real8
var m : string
var n : string(300)
var o : char
var p : char(768)
var q : addressint

%%% args: --only_parser -dump ast -b
%%% expected exit status: 0

%%% expected stdout:
%%% ast: [
%%% var [id:0] : { prim Boolean }
%%% var [id:1] : { prim Int }
%%% var [id:2] : { prim Int1 }
%%% var [id:3] : { prim Int2 }
%%% var [id:4] : { prim Int4 }
%%% var [id:5] : { prim Nat }
%%% var [id:6] : { prim Nat1 }
%%% var [id:7] : { prim Nat2 }
%%% var [id:8] : { prim Nat4 }
%%% var [id:9] : { prim Real }
%%% var [id:10] : { prim Real4 }
%%% var [id:11] : { prim Real8 }
%%% var [id:12] : { prim String_ }
%%% var [id:13] : { string(nat(300)) }
%%% var [id:14] : { prim Char }
%%% var [id:15] : { char(nat(768)) }
%%% var [id:16] : { prim AddressInt }
%%% ]

%%% expected stderr:
