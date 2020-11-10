% Dropped value
a := 
begin end % sync
a = 

%%% args: --only_parser -dump ast -b
%%% expected exit status: 255

%%% expected stdout:
%%% ast: [
%%% ref(id:0) := <error>
%%% {}
%%% ref(id:0) := <error>
%%% ]

%%% expected stderr:
%%% error line:1 column:1-1 Expected expression before '<end of file>' 
%%% error line:3 column:1-6 Expected expression before 'begin' 
%%% warn line:4 column:3-4 '=' found, assumed it to be ':='