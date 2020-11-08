% Enclosed inner scopes can't shadow an outer scope's variables
begin
    var cant_shadow := 'eep'
    
    begin
        var cant_shadow := 'eep'
    end
end

%%% args: -M -b
%%% expected exit status: 255

%%% expected stdout:
 
%%% expected stderr:
%%% error line:6 column:13-24 'cant_shadow' has already been declared