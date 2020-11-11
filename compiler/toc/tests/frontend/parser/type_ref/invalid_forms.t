% Missing identifier after '.'
var inv : an.ident.list.
begin end

% Expression does not contain only field refs
var inv : an.ident.list.containing(1, 2, 3)
begin end

%%% args: --only_parser -dump ast -b
%%% expected exit status: 255

%%% expected stdout:
%%% ast: [
%%% var [id:1] : { ref_expr ref(id:0) . ident . list }
%%% {}
%%% var [id:2] : { ref_expr ref(id:0) . ident . list . containing(nat(1), nat(2), nat(3)) }
%%% {}
%%% ]

%%% expected stderr:
%%% error line:3 column:1-6 Missing identifier after '.'
%%% error line:6 column:11-44 Expression is not a valid type reference