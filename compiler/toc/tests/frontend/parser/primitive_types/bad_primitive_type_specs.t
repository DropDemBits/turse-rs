% Should result in the base type
% Invalid: Bigger than the maximum size
var c : string(16#10000)
var c : string(16#10001)

% Invalid: Zero length size expression
var c : char(16#0)
var c : string(16#0)

% Invalid: Dropping the right paren
var c : char(16#0

begin end % synchronize

% Invalid: No length specification
var c : string(

begin end % synchronize

% Invalid: '*' specifier is only valid in subprogram parameter declarations
var c : string(*)
var c : char(*)

% Invalid: Not a type specification
% (shouldn't parse the := "hee" nor the 'to' as it may cause phantom errors)
var c : to := 'hee'

%%% args: --only_parser -dump ast -b
%%% expected exit status: 255

%%% expected stdout:
%%% ast: {
%%%     var [id:0] : { prim String_ }
%%%     var [id:1] : { prim String_ }
%%%     var [id:2] : { prim Char }
%%%     var [id:3] : { prim String_ }
%%%     var [id:4] : { char(nat(0)) }
%%%     {}
%%%     var [id:5] : { prim String_ }
%%%     {}
%%%     var [id:6] : { prim String_ }
%%%     var [id:7] : { prim Char }
%%%     var [id:8] : { error }
%%% }

%%% expected stderr:
%%% error line:3 column:16-24 '65536' is larger than or equal to the maximum string length of '65536' (after including the end byte)
%%% error line:3 column:16-24 Expected ')' after length specifier
%%% error line:3 column:16-24 '16#10000' does not begin a statement or declaration
%%% error line:4 column:16-24 '65537' is larger than or equal to the maximum string length of '65536' (after including the end byte)
%%% error line:4 column:16-24 Expected ')' after length specifier
%%% error line:4 column:16-24 '16#10001' does not begin a statement or declaration
%%% error line:7 column:14-18 Invalid maximum string length of '0'
%%% error line:7 column:14-18 Expected ')' after length specifier
%%% error line:7 column:14-18 '16#0' does not begin a statement or declaration
%%% error line:8 column:16-20 Invalid maximum string length of '0'
%%% error line:8 column:16-20 Expected ')' after length specifier
%%% error line:8 column:16-20 '16#0' does not begin a statement or declaration
%%% error line:13 column:1-6 Expected ')' after length specifier
%%% error line:18 column:1-6 Expected expression before 'begin' 
%%% error line:18 column:1-6 Length specifier is not a '*' or a non-zero compile time expression
%%% error line:18 column:1-6 Expected ')' after length specifier
%%% error line:21 column:16-17 Length specifier of '*' is only valid in subprogram parameter types
%%% error line:22 column:14-15 Length specifier of '*' is only valid in subprogram parameter types
%%% error line:26 column:9-11 Expected expression before 'to' 
%%% error line:26 column:9-11 Unexpected 'to', expected a type specifier
%%% error line:26 column:9-11 'to' does not begin a statement or declaration