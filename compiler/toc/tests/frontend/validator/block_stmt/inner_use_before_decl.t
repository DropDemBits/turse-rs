% Inner scope can't access identifiers declared after it
begin
    var fail_to_use := cant_see_me
end

var cant_see_me := 'eep'

%%% args: -M -b
%%% expected exit status: 255

%%% expected stdout:
 
%%% expected stderr:
%%% error line:3 column:24-35 'cant_see_me' has not been declared yet