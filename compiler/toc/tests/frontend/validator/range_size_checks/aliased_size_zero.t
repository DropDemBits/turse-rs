% 0 sized ranges can't hide behind aliases
begin
    begin    type a : 16#80000000 .. 16#7fffffff    var b : a                  end
    begin    type a : true .. false                 var b : a                  end
    begin    type a : 'D' .. 'C'                    var b : a                  end
    begin    type a : 16#80000000 .. 16#7fffffff    type b : set of a          end
    begin    type a : true .. false                 type b : set of a          end
    begin    type a : 'D' .. 'C'                    type b : set of a          end
    begin    type a : -1+2 .. 1-1                   type b : set of a          end
    begin    type a : 'D' .. 'C'                    type b : array a of int    end
end

% 0 sized ranges can't hide behind double aliases
begin
    type a : 'D' .. 'C'
    type b : a
    
    var c : a
    type d : set of a
    type e : array a of int
end

%%% args: -M -b
%%% expected exit status: 255

%%% expected stdout:

%%% expected stderr:
%%% error line:3 column:23-49 Range bounds creates a zero-sized range
%%% error line:4 column:23-36 Range bounds creates a zero-sized range
%%% error line:5 column:23-33 Range bounds creates a zero-sized range
%%% error line:6 column:23-49 Range bounds creates a zero-sized range
%%% error line:7 column:23-36 Range bounds creates a zero-sized range
%%% error line:8 column:23-33 Range bounds creates a zero-sized range
%%% error line:9 column:23-34 Range bounds creates a zero-sized range
%%% error line:10 column:23-33 Range bounds creates a zero-sized range
%%% error line:15 column:14-24 Range bounds creates a zero-sized range
%%% error line:15 column:14-24 Range bounds creates a zero-sized range
%%% error line:15 column:14-24 Range bounds creates a zero-sized range