% Allowed to be used in reference position
int @ (0) := 1
char(1) @ (0) := 1
string(1) @ (0) := 1
bambam @ (0) := 1

%%% args: --only_parser -dump ast -b
%%% expected exit status: 0

%%% expected stdout:
%%% ast: {
%%%     [{ prim Int }] @ (nat(0)) := nat(1)
%%%     [{ char(nat(1)) }] @ (nat(0)) := nat(1)
%%%     [{ string(nat(1)) }] @ (nat(0)) := nat(1)
%%%     [{ ref_expr ref(id:0) }] @ (nat(0)) := nat(1)
%%% }

%%% expected stderr:
