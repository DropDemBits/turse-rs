% All of these should produce errors
var beebee := 1 shl -1
beebee := 1 shr amt
beebee := 1 div 0
beebee := 1 / 0
beebee := 1 rem 0
beebee := 1 mod 0

%%% args: -M -b
%%% expected exit status: 255

%%% expected stderr:
%%% error line:2 column:21-23 Negative shift amount in compile-time 'shl' expression
%%% error line:3 column:17-20 'amt' has not been declared yet
%%% error line:5 column:11-16 Assignment value is the wrong type