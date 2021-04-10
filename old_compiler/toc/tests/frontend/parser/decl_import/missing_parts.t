% Missing closing paren
import (a
begin end

% Missing last import item
import a,
begin end

% Empty import list is okay, but only with parens
import
begin end

%%% args: --only_parser -dump ast -dump scope -b

%%% expected stdout:
%%% ast: {
%%%     import ( [id:0]  )
%%%     {}
%%%     import ( [id:1]  )
%%%     {}
%%%     import (  )
%%%     {}
%%% }
%%% scope: [
%%%        0 -> { a ty: ty_unknown, used: 0, const decl }
%%%        1 -> { a ty: ty_unknown, used: 0, const decl }
%%% ]

%%% expected stderr:
%%% error line:3 column:1-6 Expected ')' after last import item
%%% error line:6 column:1-7 Import statements are not allowed here
%%% error line:7 column:1-6 Expected an identifier or a string literal
%%% error line:10 column:1-7 Import statements are not allowed here
%%% error line:11 column:1-6 Expected an identifier or a string literal