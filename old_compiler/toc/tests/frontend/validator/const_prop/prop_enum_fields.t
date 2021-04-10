% Constant propogation should allow enum fields to be hidden behind constant vars

type e0 : enum(a, b, c)

const a : e0 := e0.a
var b : a .. e0.b := e0.c

const c := e0.a
var d : c .. e0.b := e0.c

%%% args: -M -dump ast -b
%%% expected exit status: 0

%%% expected stdout:
%%% ast: {
%%%     type [id:0] : { enum ( a, b, c, ) }
%%%     const [id:1] : { ref_expr ref(id:0) } := nat(0)
%%%     var [id:2] : { range nat(0) .. nat(1) } := nat(2)
%%%     const [id:3] := nat(0)
%%%     var [id:4] : { range nat(0) .. nat(1) } := nat(2)
%%% }

%%% expected stderr:
