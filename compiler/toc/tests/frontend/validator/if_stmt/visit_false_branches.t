% False branches should be visited
if true then
elsif true then
    a
end if

if true then
else
    b
end if

%%% args: -b
%%% expected exit status: 255
%%% expected stdout:
 
%%% expected stderr:
%%% error line:4 column:5-6 'a' has not been declared yet
%%% error line:4 column:5-6 'a' cannot be called or have subscripts
%%% error line:9 column:5-6 'b' has not been declared yet
%%% error line:9 column:5-6 'b' cannot be called or have subscripts
