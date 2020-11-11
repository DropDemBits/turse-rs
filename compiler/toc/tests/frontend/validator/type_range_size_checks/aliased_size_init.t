% `init`-sized ranges can't hide behind aliases
begin
    type a : 1 .. *

    var b : array a of int := init(1)
    type c : set of a
    var d : a
end

% `init`-sized ranges can't hide behind double aliases
begin
    type a : 1 .. *
    type b : a

    var c : array b of int
    type d : set of b
    var e : b
end

%%% args: -M -b
%%% expected exit status: 255

%%% expected stdout:

%%% expected stderr:
%%% error line:3 column:19-20 '*' as a range end is only valid in an array range
%%% error line:5 column:31-35 'init' initializers are not allowed for dynamic arrays
%%% error line:12 column:19-20 '*' as a range end is only valid in an array range