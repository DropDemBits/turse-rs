% All valid types
const c := 1 + 2 - 3
type tyref : int

var a00 := addressint @ (0)
var a01 := char       @ (0)
var a02 := char(c)    @ (0)
var a03 := string     @ (0)
var a04 := string(c)  @ (0)
var a05 := boolean    @ (0)
var a06 := int        @ (0)
var a07 := int1       @ (0)
var a08 := int2       @ (0)
var a09 := int4       @ (0)
var a10 := nat        @ (0)
var a11 := nat1       @ (0)
var a12 := nat2       @ (0)
var a13 := nat4       @ (0)
var a14 := real       @ (0)
var a15 := real4      @ (0)
var a16 := real8      @ (0)
var a17 := tyref      @ (0)

%%% args: --only_parser -dump ast -b
%%% expected exit status: 0

%%% expected stdout:
%%% ast: [
%%% const [id:0] := nat(1) + nat(2) - nat(3)
%%% type [id:1] : { prim Int }
%%% var [id:2] := [{ prim AddressInt }] @ (nat(0))
%%% var [id:3] := [{ prim Char }] @ (nat(0))
%%% var [id:4] := [{ char(ref(id:0)) }] @ (nat(0))
%%% var [id:5] := [{ prim String_ }] @ (nat(0))
%%% var [id:6] := [{ string(ref(id:0)) }] @ (nat(0))
%%% var [id:7] := [{ prim Boolean }] @ (nat(0))
%%% var [id:8] := [{ prim Int }] @ (nat(0))
%%% var [id:9] := [{ prim Int1 }] @ (nat(0))
%%% var [id:10] := [{ prim Int2 }] @ (nat(0))
%%% var [id:11] := [{ prim Int4 }] @ (nat(0))
%%% var [id:12] := [{ prim Nat }] @ (nat(0))
%%% var [id:13] := [{ prim Nat1 }] @ (nat(0))
%%% var [id:14] := [{ prim Nat2 }] @ (nat(0))
%%% var [id:15] := [{ prim Nat4 }] @ (nat(0))
%%% var [id:16] := [{ prim Real }] @ (nat(0))
%%% var [id:17] := [{ prim Real4 }] @ (nat(0))
%%% var [id:18] := [{ prim Real8 }] @ (nat(0))
%%% var [id:19] := [{ ref_expr ref(id:1) }] @ (nat(0))
%%% ]

%%% expected stderr:
