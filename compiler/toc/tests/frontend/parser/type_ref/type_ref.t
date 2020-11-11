% Identifier reference (resolved at validation time)
% Undeclared type identifiers don't produce an error until the validator stage
var c : some_type
var some_external_use : some.thing.with.these.given.fields := 3
var ranged_external : some.thing.with.start .. some.thing.with.end_thing := 5
var implicit_external : array 1 .. some.thing.with.end_thing of int

%%% args: --only_parser -dump ast -b
%%% expected exit status: 0

%%% expected stdout:
%%% ast: [
%%% var [id:1] : { ref_expr ref(id:0) }
%%% var [id:3] : { ref_expr ref(id:2) . thing . with . these . given . fields } := nat(3)
%%% var [id:4] : { range ref(id:2) . thing . with . start .. ref(id:2) . thing . with . end_thing } := nat(5)
%%% var [id:5] : { array { range nat(1) .. ref(id:2) . thing . with . end_thing } of { prim Int } }
%%% ]

%%% expected stderr:
