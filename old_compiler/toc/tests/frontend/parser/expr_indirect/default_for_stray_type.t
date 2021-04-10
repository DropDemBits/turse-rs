% Indirect expressions must be produced in error case
var a00 := addressint
var a01 := char      
var a02 := char(c)   
var a03 := string    
var a04 := string(c) 
var a05 := boolean   
var a06 := int       
var a07 := int1      
var a08 := int2      
var a09 := int4      
var a10 := nat       
var a11 := nat1      
var a12 := nat2      
var a13 := nat4      
var a14 := real      
var a15 := real4     
var a16 := real8     

%%% args: --only_parser -dump ast -b
%%% expected exit status: 255

%%% expected stdout:
%%% ast: {
%%%     var [id:0] := [{ prim AddressInt }] @ (<error>)
%%%     var [id:1] := [{ prim Char }] @ (<error>)
%%%     var [id:3] := [{ char(ref(id:2)) }] @ (<error>)
%%%     var [id:4] := [{ prim String_ }] @ (<error>)
%%%     var [id:5] := [{ string(ref(id:2)) }] @ (<error>)
%%%     var [id:6] := [{ prim Boolean }] @ (<error>)
%%%     var [id:7] := [{ prim Int }] @ (<error>)
%%%     var [id:8] := [{ prim Int1 }] @ (<error>)
%%%     var [id:9] := [{ prim Int2 }] @ (<error>)
%%%     var [id:10] := [{ prim Int4 }] @ (<error>)
%%%     var [id:11] := [{ prim Nat }] @ (<error>)
%%%     var [id:12] := [{ prim Nat1 }] @ (<error>)
%%%     var [id:13] := [{ prim Nat2 }] @ (<error>)
%%%     var [id:14] := [{ prim Nat4 }] @ (<error>)
%%%     var [id:15] := [{ prim Real }] @ (<error>)
%%%     var [id:16] := [{ prim Real4 }] @ (<error>)
%%%     var [id:17] := [{ prim Real8 }] @ (<error>)
%%% }

%%% expected stderr:
%%% error line:3 column:1-4 Expected '@' after primitive type (to form an indirection expression)
%%% error line:4 column:1-4 Expected '@' after primitive type (to form an indirection expression)
%%% error line:5 column:1-4 Expected '@' after primitive type (to form an indirection expression)
%%% error line:6 column:1-4 Expected '@' after primitive type (to form an indirection expression)
%%% error line:7 column:1-4 Expected '@' after primitive type (to form an indirection expression)
%%% error line:8 column:1-4 Expected '@' after primitive type (to form an indirection expression)
%%% error line:9 column:1-4 Expected '@' after primitive type (to form an indirection expression)
%%% error line:10 column:1-4 Expected '@' after primitive type (to form an indirection expression)
%%% error line:11 column:1-4 Expected '@' after primitive type (to form an indirection expression)
%%% error line:12 column:1-4 Expected '@' after primitive type (to form an indirection expression)
%%% error line:13 column:1-4 Expected '@' after primitive type (to form an indirection expression)
%%% error line:14 column:1-4 Expected '@' after primitive type (to form an indirection expression)
%%% error line:15 column:1-4 Expected '@' after primitive type (to form an indirection expression)
%%% error line:16 column:1-4 Expected '@' after primitive type (to form an indirection expression)
%%% error line:17 column:1-4 Expected '@' after primitive type (to form an indirection expression)
%%% error line:18 column:1-4 Expected '@' after primitive type (to form an indirection expression)
%%% error line:62 column:1-1 Expected '@' after primitive type (to form an indirection expression)
