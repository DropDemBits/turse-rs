% Not matching counts
type e : enum(a, b, c)

var gt : array 1 .. 3 of int := init(1, 2, 3, 4, 5)
var lt : array 1 .. 3 of int := init(1, 2)
% Not matching, but should handle ExprKind::Error
var err : array 1 .. 3 of int := init(1, 2, 3, or)
% Nothing, error should be reported by parser
var emp : array 1 .. 3 of int := init()

% Mismatch of scalar count
% Range
begin
    var s_a : array 1 .. 2 of int := init(1)
    var s_b : array 1 .. 2 of int := init(1, 2, 3)
end

% Boolean
begin
    var s_a : array boolean of int := init(1)
    var s_b : array boolean of int := init(1, 2, 3)
end

% Enum
begin
    var s_a : array e of int := init(1, 2)
    var s_b : array e of int := init(1, 2, 3, 4)
end

% Mismatch of compounded count
% Range
begin
    var c_a : array 1 .. 2, 3 .. 4 of int := init(1, 2, 3, 4, 5, 6)
    var c_b : array 1 .. 2, 3 .. 4 of int := init(1, 2, 3)
    var c_c : array 1 .. 2, 3 .. 4 of int := init(1, 2)
end

% Boolean
begin
    var c_a : array boolean, boolean of int := init(1, 2, 3, 4, 5, 6)
    var c_b : array boolean, boolean of int := init(1, 2, 3)
    var c_c : array boolean, boolean of int := init(1, 2)
end

% Enum
begin
    var c_a : array e, e of int := init(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
    var c_b : array e, e of int := init(1, 2, 3, 4, 5, 6)
    var c_c : array e, e of int := init(1, 2, 3)
end

%%% args: -M -b

%%% expected stderr:
%%% error line:4 column:47-48 Too many initializer values
%%% error line:5 column:41-42 Too few initializer values
%%% error line:7 column:48-50 Expected expression before 'or' 
%%% error line:7 column:48-50 Expected ')' after the last expression
%%% error line:7 column:48-50 'or' does not begin a statement or declaration
%%% error line:7 column:48-50 Expression is not a compile-time expression
%%% error line:7 column:48-50 Too many initializer values
%%% error line:9 column:39-40 Expected expression before ')' 
%%% error line:9 column:39-40 Expression is not a compile-time expression
%%% error line:9 column:39-40 Too few initializer values
%%% error line:14 column:43-44 Too few initializer values
%%% error line:15 column:49-50 Too many initializer values
%%% error line:20 column:44-45 Too few initializer values
%%% error line:21 column:50-51 Too many initializer values
%%% error line:26 column:41-42 Too few initializer values
%%% error line:27 column:47-48 Too many initializer values
%%% error line:33 column:63-64 Too many initializer values
%%% error line:34 column:57-58 Too few initializer values
%%% error line:35 column:54-55 Too few initializer values
%%% error line:40 column:65-66 Too many initializer values
%%% error line:41 column:59-60 Too few initializer values
%%% error line:42 column:56-57 Too few initializer values
%%% error line:47 column:68-70 Too many initializer values
%%% error line:48 column:56-57 Too few initializer values
%%% error line:49 column:47-48 Too few initializer values