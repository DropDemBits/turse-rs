% Constant folding length specifiers should still preserve parser validation semantics
% On error, should resolve into the base types

% Case: Zero Size
begin
    const sz := 0
    const c : char(sz + 1 - 1) := 'a'
    const s : string(sz) := 'aaa'
end

% Case: Negative size
begin
    const sz := -2
    const c : char(5 - 10) := 'a'
    const s : string(sz) := 'aaa'
end

% Case: Too large
begin
    const sz := 16#10000 - 5 + 5
    const c : char(65530 + 10) := 'a'
    const s : string(sz) := 'aaa'
end

% Case: Wrong compile-time type
begin
    const c : char(65530 + 0.0) := 'a'
    const s : string('noop' + 'boop') := 'a'
end

% Case: Not actually a compile-time expression
begin
    var depend := 65530
    const c : char(depend) := 'a'
    const s : string(1 + depend + 1) := 'a'
end

%%% args: -M -dump scope -dump ast -b
%%% expected exit status: 255

%%% expected stdout:
%%% ast: [
%%% {
%%%     const [id:0] := nat(0)
%%%     const [id:1] : { char(ref(id:0) + nat(1) - nat(1)) } := "a"
%%%     const [id:2] : { string(ref(id:0)) } := "aaa"
%%% }
%%% {
%%%     const [id:3] := int(-2)
%%%     const [id:4] : { char(nat(5) - nat(10)) } := "a"
%%%     const [id:5] : { string(ref(id:3)) } := "aaa"
%%% }
%%% {
%%%     const [id:6] := nat(65536)
%%%     const [id:7] : { char(nat(65530) + nat(10)) } := "a"
%%%     const [id:8] : { string(ref(id:6)) } := "aaa"
%%% }
%%% {
%%%     const [id:9] : { char(nat(65530) + real(0)) } := "a"
%%%     const [id:10] : { string('noop' + 'boop') } := "a"
%%% }
%%% {
%%%     var [id:11] := nat(65530)
%%%     const [id:12] : { char(ref(id:11)) } := "a"
%%%     const [id:13] : { string(nat(1) + ref(id:11) + nat(1)) } := "a"
%%% }
%%% ]
%%% scope: [
%%%        0 -> { sz ty: ty_prim[Int], used: 2, const decl comp_eval }
%%%        1 -> { c ty: ty_prim[Char], used: 0, const decl comp_eval }
%%%        2 -> { s ty: ty_prim[String_], used: 0, const decl comp_eval }
%%%        3 -> { sz ty: ty_prim[Int], used: 1, const decl comp_eval }
%%%        4 -> { c ty: ty_prim[Char], used: 0, const decl comp_eval }
%%%        5 -> { s ty: ty_prim[String_], used: 0, const decl comp_eval }
%%%        6 -> { sz ty: ty_prim[Int], used: 1, const decl comp_eval }
%%%        7 -> { c ty: ty_prim[Char], used: 0, const decl comp_eval }
%%%        8 -> { s ty: ty_prim[String_], used: 0, const decl comp_eval }
%%%        9 -> { c ty: ty_prim[Char], used: 0, const decl comp_eval }
%%%       10 -> { s ty: ty_prim[String_], used: 0, const decl comp_eval }
%%%       11 -> { depend ty: ty_prim[Int], used: 2, var decl }
%%%       12 -> { c ty: ty_prim[Char], used: 0, const decl comp_eval }
%%%       13 -> { s ty: ty_prim[String_], used: 0, const decl comp_eval }
%%% ]

%%% expected stderr:
%%% error line:7 column:20-30 Expression results in a length of 0
%%% error line:8 column:22-24 Expression results in a length of 0
%%% error line:14 column:20-26 Expression results in a negative length
%%% error line:15 column:22-24 Expression results in a negative length
%%% error line:21 column:20-30 '65540' is larger than or equal to the maximum length of 65536
%%% error line:22 column:22-24 '65536' is larger than or equal to the maximum length of 65536
%%% error line:27 column:20-31 Wrong type for a length specifier
%%% error line:28 column:22-37 Wrong type for a length specifier
%%% error line:34 column:20-26 Length specifier is not a compile-time expression
%%% error line:35 column:22-36 Length specifier is not a compile-time expression