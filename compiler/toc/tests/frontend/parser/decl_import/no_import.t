% Can import nothing
import ()

%%% args: --only_parser -dump ast -dump scope -b
%%% expected exit status: 0

%%% expected stdout:
%%% ast: {
%%%     import (  )
%%% }
%%% scope: [
%%% ]

%%% expected stderr:
