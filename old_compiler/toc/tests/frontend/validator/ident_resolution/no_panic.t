% None of these should cause a panic (all of them look weird because they're from fuzzing)
begin
    var a:0
    a
end

begin
    var a
    a
end

begin
    a =
    begin
        a
    end
end

begin
k().c
end

begin
    a(
    begin
        a
    end
end

begin
    a.
    begin
        a
    end
end

begin
    var ye0
    var y : ye0
    begin
        i=y
    end
end

begin
    var ye0
    var ye0 : ye0
    begin
        ye0=
    end
end

% Type parsing should preserve used identifiers
begin
    var k : set a
    begin
        a
    end
end

%%% args: -M -b
%%% expected exit status: 255

%%% expected stderr:
%%% error line:3 column:11-12 Expression is not a valid type reference
%%% error line:4 column:5-6 'a' cannot be called or have subscripts
%%% error line:8 column:5-8 Cannot infer type for given var declaration (no type specification or initial value given)
%%% error line:9 column:5-6 'a' cannot be called or have subscripts
%%% error line:13 column:5-6 'a' has not been declared yet
%%% error line:14 column:5-10 Expected expression before 'begin' 
%%% error line:15 column:9-10 'a' has not been declared yet
%%% error line:15 column:9-10 'a' cannot be called or have subscripts
%%% error line:20 column:1-2 'k' has not been declared yet
%%% error line:20 column:1-2 'k' cannot be called or have subscripts
%%% error line:20 column:5-6 'c' cannot be called or have subscripts
%%% error line:24 column:5-6 'a' has not been declared yet
%%% error line:24 column:5-6 'a' cannot be called or have subscripts
%%% error line:25 column:5-10 Expected expression before 'begin' 
%%% error line:25 column:5-10 Missing ')' after parameter list
%%% error line:26 column:9-10 'a' has not been declared yet
%%% error line:26 column:9-10 'a' cannot be called or have subscripts
%%% error line:31 column:5-6 'a' has not been declared yet
%%% error line:31 column:5-6 'a' cannot be called or have subscripts
%%% error line:32 column:5-10 Missing identifier after '.'
%%% error line:33 column:9-10 'a' has not been declared yet
%%% error line:33 column:9-10 'a' cannot be called or have subscripts
%%% error line:38 column:5-8 Cannot infer type for given var declaration (no type specification or initial value given)
%%% error line:39 column:13-16 'ye0' does not refer to a type
%%% error line:41 column:9-10 'i' has not been declared yet
%%% error line:46 column:5-8 Cannot infer type for given var declaration (no type specification or initial value given)
%%% error line:47 column:9-12 'ye0' has already been declared
%%% error line:47 column:15-18 'ye0' does not refer to a type
%%% error line:50 column:5-8 Expected expression before 'end' 
%%% error line:55 column:13-18 Set types can only be declared inside of 'type' statements
%%% error line:55 column:17-18 Expected 'of' after 'set'
%%% error line:55 column:17-18 'a' has not been declared yet
%%% error line:55 column:17-18 'a' does not refer to a type
%%% error line:57 column:9-10 'a' has not been declared yet
%%% error line:57 column:9-10 'a' cannot be called or have subscripts