% Enclosed inner scope can't access identifiers declared after it
begin
    begin
        var fail_to_use := cant_see_me
    end
end

var cant_see_me := 'eep'

%%% args: -M -b
%%% expected exit status: 255

%%% expected stdout:
 
%%% expected stderr:
%%% error line:4 column:28-39 'cant_see_me' has not been declared yet
