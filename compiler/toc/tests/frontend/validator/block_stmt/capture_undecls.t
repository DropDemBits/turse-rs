% Undeclared identifier should be captured

begin
    a
end

%%% args: -b
%%% expected exit status: 255

%%% expected stdout:
 
%%% expected stderr:
%%% error line:4 column:5-6 'a' has not been declared yet
%%% error line:4 column:5-6 'a' cannot be called or have subscripts