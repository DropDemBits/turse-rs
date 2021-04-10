% Only boolean expressions are allowed in the conditional
if 1 then var k := 2 k := k end if
if 1.0 then var k := 2 k := k end if
if 'yee' then var k := 2 k := k end if
if nil then var k := 2 k := k end if

if true then elsif 1 then var k := 2 k := k end if
if true then elsif 1.0 then var k := 2 k := k end if
if true then elsif 'yee' then var k := 2 k := k end if
if true then elsif nil then var k := 2 k := k end if

%%% args: -b
%%% expected exit status: 255
%%% expected stdout:
 
%%% expected stderr:
%%% error line:2 column:4-5 If condition expression is not the right type
%%% error line:3 column:4-7 If condition expression is not the right type
%%% error line:4 column:4-9 If condition expression is not the right type
%%% error line:5 column:4-7 If condition expression is not the right type
%%% error line:7 column:20-21 If condition expression is not the right type
%%% error line:8 column:20-23 If condition expression is not the right type
%%% error line:9 column:20-25 If condition expression is not the right type
%%% error line:10 column:20-23 If condition expression is not the right type