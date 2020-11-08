% Should be as many elements as specified by the array ranges
type e : enum(a, b, c)

% Match of scalar count
var s_range : array 1 .. 3 of int := init(1, 2, 3)
var s_bool  : array boolean of int := init(1, 2)
var s_enum  : array e of int := init(1, 2, 3)

% Match of compounded count
var c_range : array 1 .. 2, 1 .. 2 of int := init(1, 2, 3, 4)
var c_bool  : array boolean, boolean of int := init(1, 2, 3, 4)
var c_enum  : array e, e of int := init(1, 2, 3, 4, 5, 6, 7, 8, 9)

%%% args: -M -b
%%% expected exit status: 0

%%% expected stderr:
