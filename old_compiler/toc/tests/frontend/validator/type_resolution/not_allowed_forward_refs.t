% Forward refs aren't allowed anywhere other than as pointer types

% Not allowed here
begin
    type a : forward
    type k : a
    type a : int
end

% Not allowed here
begin
    type a : forward
    type k : set of a
    type a : int
end

%%% args: -M -b
%%% expected exit status: 255

%%% expected stdout:

%%% expected stderr:
%%% error line:6 column:14-15 Type reference is required to be resolved at this point
%%% error line:13 column:21-22 Type reference is required to be resolved at this point