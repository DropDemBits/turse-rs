% Prefix operators in infix position
const a := 1 ~ 
const a := 1 not 
const a := 1 # 
const a := 1 1.0 
const a := 1 1 
const a := 1 "keke"
const a := 1 'keke'
const a := 1 true
const a := 1 false
const a := 1 nil

%%% args: --only_parser -dump ast -b
%%% expected exit status: 255

%%% expected stdout:
%%% ast: {
%%%     const [id:0] := nat(1)
%%%     const [id:1] := nat(1)
%%%     const [id:2] := nat(1)
%%%     const [id:3] := nat(1)
%%%     const [id:4] := nat(1)
%%%     const [id:5] := nat(1)
%%%     const [id:6] := nat(1)
%%%     const [id:7] := nat(1)
%%%     const [id:8] := nat(1)
%%%     const [id:9] := nat(1)
%%% }

%%% expected stderr:
%%% error line:2 column:14-15 '~' cannot be used as an infix operator
%%% error line:2 column:14-15 '~' does not begin a statement or declaration
%%% error line:3 column:14-17 'not' cannot be used as an infix operator
%%% error line:3 column:14-17 'not' does not begin a statement or declaration
%%% error line:4 column:14-15 '#' cannot be used as an infix operator
%%% error line:4 column:14-15 '#' does not begin a statement or declaration
%%% error line:5 column:14-17 '1.0' does not begin a statement or declaration
%%% error line:6 column:14-15 '1' does not begin a statement or declaration
%%% error line:7 column:14-20 '"keke"' does not begin a statement or declaration
%%% error line:8 column:14-20 ''keke'' does not begin a statement or declaration
%%% error line:9 column:14-18 'true' does not begin a statement or declaration
%%% error line:10 column:14-19 'false' does not begin a statement or declaration
%%% error line:11 column:14-17 'nil' does not begin a statement or declaration
