% Set constructor - Not in set

% Type compatibility
type e0 : enum (a, b, c, d)
type e : enum (a, b, c, d)

type bs : set of false .. false
type is : set of 1 .. 2
type cs : set of 'c' .. 'd'
type efs : set of e.a .. e.c
type es : set of e

var s_bs : bs := bs(false)
var s_is : is := is(1)
var s_cs : cs := cs('d')
var s_efs : efs := efs(e.a)
var s_es : es := es(e.d)

% Elements from outside of set aren't allowed
s_bs := bs(true)
s_is := is(2, 3)
s_is := is(3, 4)
s_cs := cs('d', 'e')
s_cs := cs('f', 'e')
s_efs := efs(e.a, e.d)
s_efs := efs(e.d, e.d)
s_es := es(e0.a, e0.b, e0.c, e0.d)
s_es := es(e0.a, e0.b, e.c, e.d)

%%% args: -b
%%% expected exit status: 255

%%% expected stderr:
%%% error line:27 column:12-16 Element parameter is not compatible with set element type
%%% error line:27 column:18-22 Element parameter is not compatible with set element type
%%% error line:27 column:24-28 Element parameter is not compatible with set element type
%%% error line:27 column:30-34 Element parameter is not compatible with set element type
%%% error line:28 column:12-16 Element parameter is not compatible with set element type
%%% error line:28 column:18-22 Element parameter is not compatible with set element type