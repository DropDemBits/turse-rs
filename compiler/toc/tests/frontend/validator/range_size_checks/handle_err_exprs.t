% Still be safe when handling error expressions
begin type e set of(0..     end
begin type e..              end
begin type e:..0*0          end
begin var a: array of int   end

%%% args: -M -b
%%% expected exit status: 255

%%% expected stdout:

%%% expected stderr:
%%% error line:2 column:14-17 Expected ':' after identifier
%%% error line:2 column:22-24 Expected ')' to close off parenthetical grouping
%%% error line:2 column:22-24 Expected expression after '..'
%%% error line:2 column:29-32 Expected expression before 'end' 
%%% error line:3 column:13-15 Expected ':' after identifier
%%% error line:3 column:13-15 Expected expression before '..' 
%%% error line:3 column:13-15 Unexpected '..', expected a type specifier
%%% error line:3 column:13-15 '..' does not begin a statement or declaration
%%% error line:4 column:14-16 Expected expression before '..' 
%%% error line:4 column:14-16 Unexpected '..', expected a type specifier
%%% error line:4 column:14-16 '..' does not begin a statement or declaration
%%% error line:5 column:20-22 Expected expression before 'of' 
%%% error line:5 column:20-22 Expected a range specifier after ','
%%% error line:5 column:20-22 Expected a range specifier after 'array'