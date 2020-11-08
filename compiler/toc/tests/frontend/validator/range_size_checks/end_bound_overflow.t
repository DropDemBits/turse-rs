% End range overflows constitute a valid range (but emits a _warning_)
const s0 : int := 0
const s1 : int := 1

var a : -8000 .. 16#8000000000000000
var b : -1 .. 16#ffffffffffffffff
var c : 0 .. 16#ffffffffffffffff
var d : 1 .. 16#ffffffffffffffff
var e : 0-2+2 .. 16#8000000000000000
var f : 1-2+2 .. 16#8000000000000000
var g : s1 .. 16#ffffffffffffffff
var h : s0 .. 16#fffffffffffffffe
var i : s0 .. 16#ffffffffffffffff
var j : -16#7fffffffffffffff - 1 .. 16#ffffffffffffffff

% Suppress unused identifier warnings
a := a
b := b
c := c
d := d
e := e
f := f
g := g
h := h
i := i
j := j

%%% args: -b
%%% expected exit status: 0

%%% expected stdout:

%%% expected stderr:
%%% warn line:6 column:9-34 Range bound size exceeds the maximum representable size
%%% warn line:7 column:9-33 Range bound size exceeds the maximum representable size
%%% warn line:13 column:9-34 Range bound size exceeds the maximum representable size
%%% warn line:14 column:9-56 Range bound size exceeds the maximum representable size
