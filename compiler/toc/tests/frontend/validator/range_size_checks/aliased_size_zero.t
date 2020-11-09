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
%%% error line:3 column:61-62 Range bounds creates a zero-sized range
%%% error line:4 column:61-62 Range bounds creates a zero-sized range
%%% error line:5 column:61-62 Range bounds creates a zero-sized range
%%% error line:6 column:69-70 Range bounds creates a zero-sized range
%%% error line:7 column:69-70 Range bounds creates a zero-sized range
%%% error line:8 column:69-70 Range bounds creates a zero-sized range
%%% error line:9 column:69-70 Range bounds creates a zero-sized range
%%% error line:10 column:68-69 Range bounds creates a zero-sized range
%%% error line:18 column:13-14 Range bounds creates a zero-sized range
%%% error line:19 column:21-22 Range bounds creates a zero-sized range
%%% error line:20 column:20-21 Range bounds creates a zero-sized range