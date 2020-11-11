% Flexible array cannot have an implicit range
var inv : flexible array 1 .. * of real

% Array cannot have a flexible array as an element type
var inv : flexible array 1 .. 2 of flexible array 1 .. 2 of real
var inv : array 1 .. 2 of flexible array 1 .. 2 of real
var inv : array 1 .. * of flexible array 1 .. 2 of real

% Array cannot have an implicit size array as an element type
var inv : flexible array 1 .. 2 array of 1 .. * of real
var inv : flexible array 1 .. 2 of array 1 .. * of real
var inv : array 1 .. 2 of array 1 .. * of real
var inv : array 1 .. * of array 1 .. * of real

% Implicit size array cannot have more than one range specifier
var inv : array 1 .. *, char of real
var inv : array 1 .. *, 1 .. *, char of real

% Implicit size range is only allowed for the first range specifier
var inv : array 1 .. 2, 1 .. *, char of real

%%% args: --only_parser -dump ast -b
%%% expected exit status: 255

%%% expected stdout:
%%% ast: [
%%% var [id:0] : { flexible array { range nat(1) .. * } of { prim Real } }
%%% var [id:1] : { flexible array { range nat(1) .. nat(2) } of { flexible array { range nat(1) .. nat(2) } of { prim Real } } }
%%% var [id:2] : { array { range nat(1) .. nat(2) } of { flexible array { range nat(1) .. nat(2) } of { prim Real } } }
%%% var [id:3] : { array { range nat(1) .. * } of { flexible array { range nat(1) .. nat(2) } of { prim Real } } }
%%% var [id:4] : { flexible array { range nat(1) .. nat(2) } of { array  of { range nat(1) .. * } } }
%%% var [id:5] : { flexible array { range nat(1) .. nat(2) } of { array { range nat(1) .. * } of { prim Real } } }
%%% var [id:6] : { array { range nat(1) .. nat(2) } of { array { range nat(1) .. * } of { prim Real } } }
%%% var [id:7] : { array { range nat(1) .. * } of { array { range nat(1) .. * } of { prim Real } } }
%%% var [id:8] : { array { range nat(1) .. * } of { prim Real } }
%%% var [id:9] : { array { range nat(1) .. * } of { prim Real } }
%%% var [id:10] : { array { range nat(1) .. nat(2) }, { range nat(1) .. * }, { prim Char } of { prim Real } }
%%% ]

%%% expected stderr:
%%% error line:2 column:11-40 Arrays with '*' as an end bound require an 'init' initializer
%%% error line:2 column:31-32 Flexible array cannot have an implicit range specifier
%%% error line:5 column:36-44 Flexible arrays are not allowed inside of array element type specifiers
%%% error line:6 column:27-35 Flexible arrays are not allowed inside of array element type specifiers
%%% error line:7 column:11-56 Arrays with '*' as an end bound require an 'init' initializer
%%% error line:7 column:27-35 Flexible arrays are not allowed inside of array element type specifiers
%%% error line:10 column:33-38 Expected 'of' after the last range specifier
%%% error line:10 column:39-41 Expected expression before 'of' 
%%% error line:10 column:39-41 Expected a range specifier after ','
%%% error line:10 column:39-41 Expected a range specifier after 'array'
%%% error line:10 column:49-51 'of' does not begin a statement or declaration
%%% error line:11 column:36-41 Implicit size arrays are not allowed inside of array element type specifiers
%%% error line:12 column:27-32 Implicit size arrays are not allowed inside of array element type specifiers
%%% error line:13 column:11-47 Arrays with '*' as an end bound require an 'init' initializer
%%% error line:13 column:27-32 Implicit size arrays are not allowed inside of array element type specifiers
%%% error line:16 column:11-37 Arrays with '*' as an end bound require an 'init' initializer
%%% error line:16 column:25-29 Extra range specifier found in implicit size array
%%% error line:17 column:11-45 Arrays with '*' as an end bound require an 'init' initializer
%%% error line:17 column:30-31 Extra range specifier found in implicit size array
%%% error line:17 column:33-37 Extra range specifier found in implicit size array
%%% error line:20 column:30-31 '*' is only valid for the first range specifier