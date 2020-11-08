% Should report redecl errors
var a : int
type a : 1 .. 3

%%% args: -M -b
%%% expected exit status: 255
%%% expected stdout:
 
%%% expected stderr:
%%% error line:3 column:6-7 'a' has already been declared