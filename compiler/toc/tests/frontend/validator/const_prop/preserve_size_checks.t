% Constant folding length specifiers should still preserve parser validation semantics
% On error, should resovle into the base types

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

%%% args: -M -dump ast -b
%%% expected exit status: 255

%%% expected stdout:
%%% ast: [
%%% {
%%%     const [id:0] : ty_prim[Int] := nat(0)
%%%     const [id:1] : ty_prim[Char] := "a"
%%%     const [id:2] : ty_prim[String_] := "aaa"
%%% }
%%% {
%%%     const [id:3] : ty_prim[Int] := int(-2)
%%%     const [id:4] : ty_prim[Char] := "a"
%%%     const [id:5] : ty_prim[String_] := "aaa"
%%% }
%%% {
%%%     const [id:6] : ty_prim[Int] := nat(65536)
%%%     const [id:7] : ty_prim[Char] := "a"
%%%     const [id:8] : ty_prim[String_] := "aaa"
%%% }
%%% {
%%%     const [id:9] : ty_prim[Char] := "a"
%%%     const [id:10] : ty_prim[String_] := "a"
%%% }
%%% {
%%%     var [id:11] : ty_prim[Int] := nat(65530)
%%%     const [id:12] : ty_prim[Char] := "a"
%%%     const [id:13] : ty_prim[String_] := "a"
%%% }
%%% ]

%%% expected stderr:
%%% error line:7 column:20-30 Invalid maximum string length of '0'
%%% error line:8 column:22-24 Invalid maximum string length of '0'
%%% error line:14 column:20-26 Compile-time string length specifier is negative
%%% error line:15 column:22-24 Compile-time string length specifier is negative
%%% error line:21 column:20-30 '65540' is larger than or equal to the maximum string length of '65536' (after including the end byte)
%%% error line:22 column:22-24 '65536' is larger than or equal to the maximum string length of '65536' (after including the end byte)
%%% error line:27 column:20-31 Wrong type for a string length specifier
%%% error line:28 column:22-37 Wrong type for a string length specifier
%%% error line:34 column:20-26 Expression is not a compile-time expression
%%% error line:35 column:22-36 Expression is not a compile-time expression