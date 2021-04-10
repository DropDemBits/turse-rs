% Aliases to aliases are allowed
type a : int
type b : a
type c : b

%%% args: -M -dump types -b
%%% expected exit status: 0

%%% expected stdout:
%%% types: [
%%%        0 -> { alias to ty_prim[Int] }
%%%        1 -> { alias to ty_prim[Int] }
%%%        2 -> { alias to ty_prim[Int] }
%%% ]

%%% expected stderr:
