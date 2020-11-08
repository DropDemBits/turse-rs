% Aliases to aliases are allowed
type a : int
type b : a
type c : b

%%% args: -M -dump types -b
%%% expected exit status: 0

%%% expected stdout:
%%% types: [
%%%        0 -> { alias to ty_prim[Int] }
%%%        1 -> { ref_expr ref(id:0) }
%%%        2 -> { alias to ty_prim[Int] }
%%%        3 -> { ref_expr ref(id:1) }
%%%        4 -> { alias to ty_prim[Int] }
%%% ]

%%% expected stderr:
