% "init" initializers are not allowed for dynamic or flexible arrays
var flex : flexible array 1 .. 3 of int := init(1, 2, 3)

var len := 3
var dyn : array 1 .. len of int := init(1, 2, 3)

%%% args: -M -b

%%% expected stderr:
%%% error line:2 column:44-48 'init' initializers are not allowed for flexible arrays
%%% error line:5 column:36-40 'init' initializers are not allowed for dynamic arrays