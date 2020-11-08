% Aliases are equivalent to their base types
% Range is allowed
begin
    type a : 1 .. 5
    type b : set of a
end

begin
    type a : int
    type b : a
    var c : int := 1
    var d : b := c
end

begin
    type a : boolean
    type b : set of a
end

begin
    type a : char
    type b : set of a
end

% Should fail
begin
    type a : real
    type b : set of a
end

% Should fail
begin
    type a : string
    type b : set of a
end
% TODO: include cases of record and union

%%% args: -M -b
%%% expected exit status: 255

%%% expected stdout:

%%% expected stderr:
%%% error line:28 column:21-22 Set index is not a range, char, boolean, or enumerated type
%%% error line:34 column:21-22 Set index is not a range, char, boolean, or enumerated type