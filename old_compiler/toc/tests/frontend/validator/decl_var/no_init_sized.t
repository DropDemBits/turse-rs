% Init-sized type specs are not allowed in const/var decls
var a : 1 .. *
const b : 1 .. *

%%% args: -M -b
%%% expected exit status: 255

%%% expected stdout:
 
%%% expected stderr:
%%% error line:2 column:14-15 '*' as a range end is only valid in an array range
%%% error line:3 column:1-6 const declaration requires an initial value
%%% error line:3 column:16-17 '*' as a range end is only valid in an array range