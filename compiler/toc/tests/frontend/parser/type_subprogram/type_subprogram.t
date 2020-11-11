% Subprogram parsing
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

%%% args: --only_parser -dump ast -b
%%% expected exit status: 0

%%% expected stdout:
%%% ast: [
%%% var [id:0] : { procedure }
%%% var [id:1] : { procedure () }
%%% var [id:2] : { procedure (a : { prim Int }) }
%%% var [id:3] : { procedure (a : { prim Int }, b : { prim String_ }) }
%%% var [id:4] : { procedure (a : { prim Int }, b : { prim String_ }, c : { procedure () }) }
%%% var [id:5] : { function () -> { prim Real } }
%%% var [id:6] : { function (a : { prim Int }) -> { prim String_ } }
%%% var [id:7] : { function (a : { prim Int }, b : { prim String_ }) -> { prim AddressInt } }
%%% var [id:8] : { function (a : { prim Int }, b : { prim String_ }, c : { procedure () }) -> { prim Boolean } }
%%% var [id:9] : { function (a : { prim Int }, b : { prim Int }, c : { prim Real }) -> { prim Int } }
%%% var [id:10] : { function (a : { prim Int }, b : { prim Int }, c : { prim Real }) -> { prim Int } }
%%% var [id:11] : { function (var a : { prim Int }, var b : { prim Int }, c : { prim String_ }) -> { prim Int } }
%%% var [id:12] : { function (var a : { prim Int }, var b : { prim Int }, c : { prim String_ }) -> { prim Int } }
%%% var [id:13] : { function (cheat var a : { prim Int }, cheat var register b : { prim Int }, c : { procedure }) -> { prim Int } }
%%% var [id:14] : { function (a : { function (a : { function -> { prim Int } }) -> { prim Int } }, b : { procedure (a : { procedure (a : { procedure (a : { procedure }) }) }) }, c : { procedure }) -> { prim Int } }
%%% ]

%%% expected stderr: