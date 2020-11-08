% Constant propogation should allow enum fields to be hidden behind constant vars

type e0 : enum(a, b, c)

const a : e0 := e0.a
var b : a .. e0.b := e0.c

const c := e0.a
var d : c .. e0.b := e0.c

%%% args: -M -dump ast -b
%%% expected exit status: 0

%%% expected stdout:
%%% ast: [
%%% type [id:0] : ty_id[4]
%%% const [id:1] : ty_id[0] := nat(0)
%%% var [id:2] : ty_id[6] := nat(2)
%%% const [id:3] : ty_id[1] := nat(0)
%%% var [id:4] : ty_id[7] := nat(2)
%%% ]

%%% expected stderr:
