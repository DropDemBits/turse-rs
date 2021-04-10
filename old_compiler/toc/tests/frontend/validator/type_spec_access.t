% Type errors should propogate downards and be handled correctly (i.e. not panic)
begin
var j : int
var k := (j.k) + 1
end

begin
var j : int
var k := # (j.k)
end

begin
var j : int
var k := # (j->k)
end

%%% args: -M -b
%%% expected exit status: 255

%%% expected stderr:
%%% error line:4 column:11-12 Left side of '.' is not a compound type
%%% error line:9 column:13-14 Left side of '.' is not a compound type
%%% error line:14 column:16-17 Arrow expressions are not validated yet