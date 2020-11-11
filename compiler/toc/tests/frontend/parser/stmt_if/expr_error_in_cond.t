% Should be able to handle expr errors in the condition fine
if + then endif
if + then else endif
if + then elsif - then endif

%%% args: --only_parser -dump ast -b
%%% expected exit status: 255

%%% expected stdout:
%%% ast: {
%%%     if (+<error>) then {}
%%%     if (+<error>) then {}
%%%     else {}
%%%     if (+<error>) then {}
%%%     else if (-<error>) then {}
%%% }

%%% expected stderr:
%%% error line:2 column:6-10 Expected expression before 'then' 
%%% error line:3 column:6-10 Expected expression before 'then' 
%%% error line:4 column:6-10 Expected expression before 'then' 
%%% error line:4 column:19-23 Expected expression before 'then' 
%%% warn line:2 column:11-16 'endif' found, assumed it to be 'end if'
%%% warn line:3 column:16-21 'endif' found, assumed it to be 'end if'
%%% warn line:4 column:24-29 'endif' found, assumed it to be 'end if'