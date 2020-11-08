% Forward refs are only allowed in pointer type definitions
% Allowed here
begin
    type a : forward
    var k : ^a
    type a : int
end

% Allowed here
begin
    type a : forward
    type k : ^a
    type a : int
end

%%% args: -M -b
%%% expected exit status: 0

%%% expected stdout:

%%% expected stderr:
