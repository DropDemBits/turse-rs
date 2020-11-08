var a : 1 .. 3 := 2
const b : 1 .. 3 := 2

%%% args: -b
%%% expected exit status: 0
%%% expected stdout:
 
%%% expected stderr:
%%% warn line:1 column:5-6 This declaration of 'a' is never used
%%% warn line:2 column:7-8 This declaration of 'b' is never used