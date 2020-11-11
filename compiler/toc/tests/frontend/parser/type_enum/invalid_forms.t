% At least one field must be specified
type a : enum () begin end
type a : enum )  begin end
type a : enum    begin end

% Right paren is required, but should not create an error type
type a : enum (a
begin end
type a : enum (a, b, c
begin end

% Field identifiers must be separated by comma delimiters (ends the list otherwise)
type a : enum (a, b
c += 1
begin end

% Non-identifiers terminate the list
type a : enum (a, to
begin end

% Enums not in top-level type contexts are rejected
% (i.e. anonymous enums are not allowed), but still produce
% an enum type
var a : enum (a, b, c)
const a : enum (a, b, c) := 2
type a : set of enum (a, b, c)

%%% args: --only_parser -dump ast -b
%%% expected exit status: 255

%%% expected stdout:
%%% ast: [
%%% type [id:0] : { enum ( , ) }
%%% {}
%%% type [id:1] : { enum ( , ) }
%%% {}
%%% type [id:2] : { enum ( , ) }
%%% {}
%%% type [id:3] : { enum ( a, ) }
%%% {}
%%% type [id:4] : { enum ( a, b, c, ) }
%%% {}
%%% type [id:5] : { enum ( a, b, ) }
%%% ref(id:6) += nat(1)
%%% {}
%%% type [id:7] : { enum ( a, , ) }
%%% {}
%%% var [id:8] : { enum ( a, b, c, ) }
%%% const [id:9] : { enum ( a, b, c, ) } := nat(2)
%%% type [id:10] : { set of { error } }
%%% ]

%%% expected stderr:
%%% error line:2 column:16-17 Expected identifier after '(' (')' is not an identifier)
%%% error line:3 column:15-16 Expected '(' after 'enum'
%%% error line:3 column:15-16 Expected identifier after 'enum' (')' is not an identifier)
%%% error line:4 column:18-23 Expected '(' after 'enum'
%%% error line:4 column:18-23 Expected identifier after 'enum' ('begin' is not an identifier)
%%% error line:4 column:18-23 Expected ')' after enumeration field declarations
%%% error line:8 column:1-6 Expected ')' after enumeration field declarations
%%% error line:10 column:1-6 Expected ')' after enumeration field declarations
%%% error line:14 column:1-2 Expected ')' after enumeration field declarations
%%% error line:18 column:19-21 Expected identifier after ',' ('to' is not an identifier)
%%% error line:18 column:19-21 Expected ')' after enumeration field declarations
%%% error line:18 column:19-21 'to' does not begin a statement or declaration
%%% error line:24 column:9-23 Enumerated types can only be declared inside of 'type' statements
%%% error line:25 column:11-25 Enumerated types can only be declared inside of 'type' statements
%%% error line:26 column:17-21 Expected expression before 'enum' 
%%% error line:26 column:17-21 'enum' does not begin a statement or declaration
