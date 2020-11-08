% Inner scopes can't shadow an outer scope's variables
var cant_shadow := 'eep'

begin
    var cant_shadow := 'eep'
end

%%% args: -M -b
%%% expected exit status: 255

%%% expected stdout:
 
%%% expected stderr:
%%% error line:5 column:9-20 'cant_shadow' has already been declared