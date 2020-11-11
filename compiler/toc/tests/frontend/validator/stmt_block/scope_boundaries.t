% Local scope identifiers don't leak out of scope boundaries
begin
    const cant_see_mee : int := 3
end

const cant_see_mee : string := 'heehee'

%%% args: -M -b
%%% expected exit status: 0

%%% expected stdout:
 
%%% expected stderr: