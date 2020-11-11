% Enums can have 1 or more fields
type enumeration : enum (a, b, c, d, e, f)
type a : enum (a)
type a : enum (a, b, c)

%%% args: --only_parser -dump ast -b
%%% expected exit status: 0

%%% expected stdout:
%%% ast: [
%%% type [id:0] : { enum ( a, b, c, d, e, f, ) }
%%% type [id:1] : { enum ( a, ) }
%%% type [id:2] : { enum ( a, b, c, ) }
%%% ]

%%% expected stderr:
