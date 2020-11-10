% Undeclared type identifiers don't produce an error until the validator stage
var a : pointer to int
var a_alt : unchecked pointer to int
var b : ^ string
var c : some_type
var d : procedure nps
var e : procedure np   ()
var f : procedure p1   (a : int)
var g : procedure p2   (a : int, b : string)
var h : procedure pisp (a : int, b : string, c : procedure _ ())
var j : function np   () : real
var k : function p1   (a : int) : string
var l : function p2   (a : int, b : string) : addressint
var m : function pisp (a : int, b : string, c : procedure _ ()) : boolean

% Pairs are to be equivalent
var n : function _ (a, b : int, c : real) : int
var o : function _ (a : int, b : int, c : real) : int
var p : function _ (var a, b : int, c : string) : int
var q : function _ (var a : int, var b : int, c : string) : int

% Other variations
var r : function _ (var a : cheat int, var register b : cheat int, proc c) : int
% Nesting fun!
% While not valid in TProlog, it should still be valid syntax as inner parameter names are ignored
var s : function _ (function a (function a : int ) : int, proc b (proc a (proc a( proc a))), proc c) : int

% Range parsing
var a_range : (1 - 3 shl 5) .. (2 * 50 - 8 * 4)

% Set parsing (only valid in type statements)
type some_set : set of 1 .. 5
type some_set_c : set of char
type some_set_b : set of boolean

% Array parsing setup
var start_range := 1
var end_range := 5

% Array parsing (enum ranges aren't parsed yet, but are equivalent to identifiers)
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

% Identifier reference (resolved at validation time)
var some_external_use : some.thing.with.these.given.fields := 3
var ranged_external : some.thing.with.start .. some.thing.with.end_thing := 5
var implicit_external : array 1 .. some.thing.with.end_thing of int

% Enum types
type enumeration : enum (a, b, c, d, e, f)

%%% args: --only_parser -dump ast -b
%%% expected exit status: 0

%%% expected stdout:
%%% ast: [
%%% var [id:0] : { pointer to { prim Int } }
%%% var [id:1] : { unchecked pointer to { prim Int } }
%%% var [id:2] : { pointer to { prim String_ } }
%%% var [id:4] : { ref_expr ref(id:3) }
%%% var [id:5] : { procedure }
%%% var [id:6] : { procedure () }
%%% var [id:7] : { procedure (a : { prim Int }) }
%%% var [id:8] : { procedure (a : { prim Int }, b : { prim String_ }) }
%%% var [id:9] : { procedure (a : { prim Int }, b : { prim String_ }, c : { procedure () }) }
%%% var [id:10] : { function () -> { prim Real } }
%%% var [id:11] : { function (a : { prim Int }) -> { prim String_ } }
%%% var [id:12] : { function (a : { prim Int }, b : { prim String_ }) -> { prim AddressInt } }
%%% var [id:13] : { function (a : { prim Int }, b : { prim String_ }, c : { procedure () }) -> { prim Boolean } }
%%% var [id:14] : { function (a : { prim Int }, b : { prim Int }, c : { prim Real }) -> { prim Int } }
%%% var [id:15] : { function (a : { prim Int }, b : { prim Int }, c : { prim Real }) -> { prim Int } }
%%% var [id:16] : { function (var a : { prim Int }, var b : { prim Int }, c : { prim String_ }) -> { prim Int } }
%%% var [id:17] : { function (var a : { prim Int }, var b : { prim Int }, c : { prim String_ }) -> { prim Int } }
%%% var [id:18] : { function (cheat var a : { prim Int }, cheat var register b : { prim Int }, c : { procedure }) -> { prim Int } }
%%% var [id:19] : { function (a : { function (a : { function -> { prim Int } }) -> { prim Int } }, b : { procedure (a : { procedure (a : { procedure (a : { procedure }) }) }) }, c : { procedure }) -> { prim Int } }
%%% var [id:20] : { range (nat(1) - nat(3) shl nat(5)) .. (nat(2) * nat(50) - nat(8) * nat(4)) }
%%% type [id:21] : { set of { range nat(1) .. nat(5) } }
%%% type [id:22] : { set of { prim Char } }
%%% type [id:23] : { set of { prim Boolean } }
%%% var [id:24] := nat(1)
%%% var [id:25] := nat(5)
%%% var [id:26] : { array { range nat(1) .. nat(2) } of { prim Int } }
%%% var [id:27] : { array { range nat(1) .. nat(2) }, { range (-nat(1) - nat(20)) .. (nat(2) + nat(3)) }, { range (nat(1) + nat(8)) .. (nat(2) + nat(16)) } of { prim String_ } }
%%% var [id:28] : { array { range 'a' .. 'f' } of { prim Real } }
%%% var [id:29] : { array { prim Char } of { prim Nat } }
%%% var [id:30] : { array { range bool(false) .. bool(true) } of { prim Char } }
%%% var [id:31] : { array { prim Boolean } of { prim Boolean } }
%%% var [id:32] : { array { range ref(id:24) .. ref(id:25) } of { prim Real } }
%%% var [id:33] : { array { range nat(1) .. * } of { prim Real } } := init(nat(1), nat(2), nat(3), nat(4), nat(5))
%%% var [id:34] : { flexible array { range nat(1) .. nat(0) } of { prim Real } }
%%% var [id:35] := nat(5)
%%% var [id:36] : { array { range nat(1) .. ref(id:35) } of { prim Real } }
%%% var [id:38] : { ref_expr ref(id:37) . thing . with . these . given . fields } := nat(3)
%%% var [id:39] : { range ref(id:37) . thing . with . start .. ref(id:37) . thing . with . end_thing } := nat(5)
%%% var [id:40] : { array { range nat(1) .. ref(id:37) . thing . with . end_thing } of { prim Int } }
%%% type [id:41] : { enum ( a, b, c, d, e, f, ) }
%%% ]

%%% expected stderr:
