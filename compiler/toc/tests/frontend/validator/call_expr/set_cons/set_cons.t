% Set constructor
% TODO is parsing 'all' token

% Type compatibility
type e : enum (a, b, c, d)

type bs : set of false .. true
type is : set of 1 .. 2
type cs : set of 'c' .. 'd'
type efs : set of e.a .. e.c
type es : set of e

% Arg count min is 1, but is unbounded & allows repeated

var s_bs : bs := bs(false)
s_bs := bs(true)
s_bs := bs(false, true)
s_bs := bs(false, true, true, false)

var s_is : is := is(1)
s_is := is(1, 2)
s_is := is(1, 2, 2, 2, 2, 1)

var s_cs : cs := cs('d')
s_cs := cs('c')
s_cs := cs('c', 'd')
s_cs := cs('c', 'd', 'd', 'd', 'd')

var s_efs : efs := efs(e.a)
s_efs := efs(e.a, e.c)
s_efs := efs(e.a, e.c, e.b)

var s_es : es := es(e.d)
s_es := es(e.d, e.d, e.d, e.a)
s_es := es(e.a, e.b, e.c, e.d)

%%% args: -b
%%% expected status code: 0

%%% expected stderr: