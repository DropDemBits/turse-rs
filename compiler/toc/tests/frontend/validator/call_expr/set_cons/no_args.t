% Set constructor - Not in set

% Type compatibility
type e : enum (a, b, c, d)

type bs : set of false .. true
type is : set of 1 .. 2
type cs : set of 'c' .. 'd'
type efs : set of e.a .. e.c
type es : set of e

% Min arg count is 1
var s_bs : bs := bs()
var s_is : is := is()
var s_cs : cs := cs()
var s_efs : efs := efs()
var s_es : es := es()

% Supress warnings
s_bs := s_bs
s_is := s_is
s_cs := s_cs
s_efs := s_efs
s_es := s_es

%%% args: -b
%%% expected exit status: 255

%%% expected stderr:
%%% error line:13 column:20-21 Set constructors require at least 1 parameter
%%% error line:14 column:20-21 Set constructors require at least 1 parameter
%%% error line:15 column:20-21 Set constructors require at least 1 parameter
%%% error line:16 column:23-24 Set constructors require at least 1 parameter
%%% error line:17 column:20-21 Set constructors require at least 1 parameter