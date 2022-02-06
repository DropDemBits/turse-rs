//! Type grammars
#[cfg(test)]
mod test;

use crate::parser::marker::Marker;

use super::*;

pub(super) fn ty(p: &mut Parser) -> Option<CompletedMarker> {
    ty_or_ty_expr(p, true)
}

/// Only to be used in sizeof_expr
pub(super) fn ty_structured(p: &mut Parser) -> Option<CompletedMarker> {
    ty_or_ty_expr(p, false)
}

pub(super) fn ty_primitive(p: &mut Parser) -> Option<CompletedMarker> {
    // TODO: Add 64 bit types (int8, nat8, long int, long nat)
    match_token!(|p| match {
        TokenKind::Addressint,
        TokenKind::Boolean,
        // Ints
        TokenKind::Int,
        TokenKind::Int1,
        TokenKind::Int2,
        TokenKind::Int4,
        // Nats
        TokenKind::Nat,
        TokenKind::Nat1,
        TokenKind::Nat2,
        TokenKind::Nat4,
        // Reals
        TokenKind::Real,
        TokenKind::Real4,
        TokenKind::Real8 => prim_type(p),
        TokenKind::Char => prim_charseq_type(p, TokenKind::Char),
        TokenKind::String_ => prim_charseq_type(p, TokenKind::String_),
        _ => None // Not a primitive type
    })
}

fn ty_or_ty_expr(p: &mut Parser, allow_ty_expr: bool) -> Option<CompletedMarker> {
    ty_primitive(p).or_else(|| {
        match_token!(|p| match {
            TokenKind::Flexible,
            TokenKind::Array => array_type(p), // array_type
            TokenKind::Enum => enum_type(p), // enum_type
            TokenKind::Unchecked,
            TokenKind::Pointer => pointer_type(p), // pointer_type
            TokenKind::Caret => short_pointer_type(p), // pointer_type (short form)
            TokenKind::Set => set_type(p), // set_type
            TokenKind::Procedure,
            TokenKind::Function => subprog_type(p), // subprog_type
            TokenKind::Record => record_type(p), // record_type
            TokenKind::Union => union_type(p), // union_type
            TokenKind::Packed => packed_type(p),
            TokenKind::Collection => collection_type(p), // collection_type
            TokenKind::Priority,
            TokenKind::Deferred,
            TokenKind::Timeout,
            TokenKind::Condition => condition_type(p), // condition_type
            _ => {
                if allow_ty_expr {
                    expr::expr(p).and_then(|cm| {
                        // either name type or range type
                        // further checks are pushed down to AST validation
                        // so e.g. int literals are allowed in type position
                        if p.at(TokenKind::Range) {
                            // range tail
                            range_type_tail(p, cm)
                        } else {
                            // Enclose expr (potential name ref) inside NameType
                            Some(cm.precede(p).complete(p, SyntaxKind::NameType))
                        }
                    })
                    .or_else(|| {
                        // not a ty
                        p.error_unexpected().with_category(Expected::Type).report();
                        None
                    })
                } else {
                    None
                }
            }
        })
    })
}

fn prim_type(p: &mut Parser) -> Option<CompletedMarker> {
    debug_assert!(
        p.at(TokenKind::Addressint)
            || p.at(TokenKind::Boolean)
            || p.at(TokenKind::Int)
            || p.at(TokenKind::Int1)
            || p.at(TokenKind::Int2)
            || p.at(TokenKind::Int4)
            || p.at(TokenKind::Nat)
            || p.at(TokenKind::Nat1)
            || p.at(TokenKind::Nat2)
            || p.at(TokenKind::Nat4)
            || p.at(TokenKind::Real)
            || p.at(TokenKind::Real4)
            || p.at(TokenKind::Real8)
    );

    let m = p.start();
    p.bump();
    Some(m.complete(p, SyntaxKind::PrimType))
}

fn prim_charseq_type(p: &mut Parser, prim_kind: TokenKind) -> Option<CompletedMarker> {
    debug_assert!(p.at(prim_kind));

    let m = p.start();
    p.bump();

    if p.at(TokenKind::LeftParen) {
        // make it sized!
        let kind = match prim_kind {
            TokenKind::Char => SyntaxKind::SizedCharType,
            TokenKind::String_ => SyntaxKind::SizedStringType,
            _ => unreachable!(),
        };

        p.bump();

        p.with_extra_recovery(&[TokenKind::RightParen], |p| {
            let m = p.start();

            if !p.eat(TokenKind::Star) {
                // if not dyn sized, parse an expr
                expr::expect_expr(p);
            }

            m.complete(p, SyntaxKind::SeqLength);
        });

        p.expect_punct(TokenKind::RightParen);

        // Wrap it inside of a `PrimType`
        let m = m.complete(p, kind).precede(p);
        Some(m.complete(p, SyntaxKind::PrimType))
    } else {
        // basic unsized type
        Some(m.complete(p, SyntaxKind::PrimType))
    }
}

fn array_type(p: &mut Parser) -> Option<CompletedMarker> {
    debug_assert!(p.at(TokenKind::Flexible) || p.at(TokenKind::Array));

    let m = p.start();

    if p.eat(TokenKind::Flexible) && !p.at(TokenKind::Array) {
        // stop, not an array type
        p.error_unexpected().with_marker(m).report();
        return None;
    }

    // on 'array'
    p.bump();

    p.with_extra_recovery(&[TokenKind::Of], |p| {
        self::range_list(p);
    });

    p.expect_punct(TokenKind::Of);

    // on index type
    self::ty(p);

    Some(m.complete(p, SyntaxKind::ArrayType))
}

fn range_list(p: &mut Parser) -> Option<CompletedMarker> {
    let m = p.start();

    p.with_extra_recovery(&[TokenKind::Comma], |p| {
        self::ty(p);

        while p.at(TokenKind::Comma) {
            p.bump();

            self::ty(p);
        }
    });

    Some(m.complete(p, SyntaxKind::RangeList))
}

fn enum_type(p: &mut Parser) -> Option<CompletedMarker> {
    debug_assert!(p.at(TokenKind::Enum));

    let m = p.start();
    p.bump();

    p.expect_punct(TokenKind::LeftParen);
    p.with_extra_recovery(&[TokenKind::RightParen], |p| {
        super::name_list(p);
    });
    p.expect_punct(TokenKind::RightParen);

    Some(m.complete(p, SyntaxKind::EnumType))
}

fn set_type(p: &mut Parser) -> Option<CompletedMarker> {
    debug_assert!(p.at(TokenKind::Set));

    let m = p.start();
    p.bump();

    p.expect_punct(TokenKind::Of);

    // parse index type
    self::ty(p);

    Some(m.complete(p, SyntaxKind::SetType))
}

fn pointer_type(p: &mut Parser) -> Option<CompletedMarker> {
    debug_assert!(p.at(TokenKind::Unchecked) || p.at(TokenKind::Pointer));

    let m = p.start();
    p.eat(TokenKind::Unchecked);

    if !p.eat(TokenKind::Caret) {
        p.expect(TokenKind::Pointer);
        p.expect_punct(TokenKind::To);
    }

    // parse pointed to type
    self::ty(p);

    Some(m.complete(p, SyntaxKind::PointerType))
}

fn short_pointer_type(p: &mut Parser) -> Option<CompletedMarker> {
    debug_assert!(p.at(TokenKind::Caret));

    let m = p.start();
    p.bump(); // nom on ^

    // parse pointed to type
    self::ty(p);

    Some(m.complete(p, SyntaxKind::PointerType))
}

// expose because it's used by param_decl
pub(super) fn subprog_type(p: &mut Parser) -> Option<CompletedMarker> {
    debug_assert!(p.at(TokenKind::Procedure) || p.at(TokenKind::Function));

    let is_fcn_ty = p.at(TokenKind::Function);

    let m = p.start();
    p.bump();

    if p.at(TokenKind::Identifier) {
        // Nom (option name)
        super::name(p);
    }

    if p.at(TokenKind::LeftParen) {
        p.with_extra_recovery(&[TokenKind::Colon], |p| {
            self::param_spec(p);
        });
    }

    if is_fcn_ty {
        // result type
        p.expect_punct(TokenKind::Colon);

        self::ty(p);
    }

    Some(m.complete(
        p,
        if is_fcn_ty {
            SyntaxKind::FcnType
        } else {
            SyntaxKind::ProcType
        },
    ))
}

// expose because it's used by param_decl
pub(super) fn constvar_param(p: &mut Parser) -> Option<CompletedMarker> {
    debug_assert!(p.at(TokenKind::Var) || p.at(TokenKind::Register) || p.at(TokenKind::Identifier));

    let m = p.start();
    attr_var(p);
    attr_register(p);

    p.with_extra_recovery(&[TokenKind::Colon], |p| {
        super::name_list(p);
    });

    p.expect_punct(TokenKind::Colon);

    // optional: `cheat`
    p.eat(TokenKind::Cheat);

    ty::ty(p);

    Some(m.complete(p, SyntaxKind::ConstVarParam))
}

fn record_type(p: &mut Parser) -> Option<CompletedMarker> {
    debug_assert!(p.at(TokenKind::Record));

    let m = p.start();
    record_type_tail(p, m)
}

fn record_type_tail(p: &mut Parser, m: Marker) -> Option<CompletedMarker> {
    debug_assert!(p.at(TokenKind::Record));

    p.bump();

    while !p.at_end() && !p.at(TokenKind::End) {
        if record_field(p).is_none() {
            // Didn't parse anything
            break;
        }
    }

    let m_end = p.start();
    p.expect(TokenKind::End);
    p.expect(TokenKind::Record);
    m_end.complete(p, SyntaxKind::EndGroup);

    Some(m.complete(p, SyntaxKind::RecordType))
}

fn record_field(p: &mut Parser) -> Option<CompletedMarker> {
    let mut parsed_any = false;

    let m = p.start();

    p.with_extra_recovery(&[TokenKind::Semicolon], |p| {
        p.with_extra_recovery(&[TokenKind::Colon], |p| {
            parsed_any |= super::name_list(p).is_some();
        });

        p.expect_punct(TokenKind::Colon);

        parsed_any |= ty::ty(p).is_some();
    });

    // Optional semicolon
    parsed_any |= p.eat(TokenKind::Semicolon);
    // Eat any optional trailing semicolons
    while p.hidden_eat(TokenKind::Semicolon) {}

    Some(m.complete(p, SyntaxKind::RecordField)).filter(|_| parsed_any)
}

fn union_type(p: &mut Parser) -> Option<CompletedMarker> {
    debug_assert!(p.at(TokenKind::Union));

    let m = p.start();
    union_type_tail(p, m)
}

fn union_type_tail(p: &mut Parser, m: Marker) -> Option<CompletedMarker> {
    debug_assert!(p.at(TokenKind::Union));

    p.bump();

    // optional: tag name
    if p.at(TokenKind::Identifier) {
        super::name(p);
    }

    p.with_extra_recovery(&[TokenKind::Label], |p| {
        // range_ty
        p.with_extra_recovery(&[TokenKind::Of], |p| {
            p.expect_punct(TokenKind::Colon);
            ty::ty(p);
        });

        p.expect_punct(TokenKind::Of);

        // variants
        while p.at(TokenKind::Label) {
            union_variant(p);
        }
    });

    let m_end = p.start();
    p.expect(TokenKind::End);
    p.expect(TokenKind::Union);
    m_end.complete(p, SyntaxKind::EndGroup);

    Some(m.complete(p, SyntaxKind::UnionType))
}

fn union_variant(p: &mut Parser) -> Option<CompletedMarker> {
    debug_assert!(p.at(TokenKind::Label));

    let m = p.start();
    p.bump(); // bump `label`

    if !p.at(TokenKind::Colon) {
        p.with_extra_recovery(&[TokenKind::Colon], |p| {
            expr::expr_list(p);
        });
    }

    p.expect_punct(TokenKind::Colon);
    // end of label portion

    // Union fields
    p.with_extra_recovery(&[TokenKind::Label], |p| {
        while !(p.at(TokenKind::End) || p.at(TokenKind::Label)) {
            if record_field(p).is_none() {
                break;
            }
        }
    });

    Some(m.complete(p, SyntaxKind::UnionVariant))
}

fn packed_type(p: &mut Parser) -> Option<CompletedMarker> {
    // 'packed' ( 'record' | 'union' )
    debug_assert!(p.at(TokenKind::Packed));

    let m = p.start();
    p.bump();

    match_token!(|p| match {
        TokenKind::Record => record_type_tail(p, m),
        TokenKind::Union => union_type_tail(p, m),
        _ => {
            // Unexpected type
            p.error_unexpected().with_marker(m).report();
            None
        }
    })
}

fn collection_type(p: &mut Parser) -> Option<CompletedMarker> {
    debug_assert!(p.at(TokenKind::Collection));

    let m = p.start();
    p.bump();
    p.expect_punct(TokenKind::Of);

    if p.eat(TokenKind::Forward) {
        super::name(p);
    } else {
        self::ty(p);
    }

    Some(m.complete(p, SyntaxKind::CollectionType))
}

fn condition_type(p: &mut Parser) -> Option<CompletedMarker> {
    debug_assert!(
        p.at(TokenKind::Priority)
            || p.at(TokenKind::Deferred)
            || p.at(TokenKind::Timeout)
            || p.at(TokenKind::Condition)
    );

    let mut m = p.start();

    if p.eat(TokenKind::Priority) || p.eat(TokenKind::Deferred) || p.eat(TokenKind::Timeout) {
        // wrap the kind in a wrapper node
        m = m.complete(p, SyntaxKind::ConditionKind).precede(p);

        // expect condition after attribute
        p.expect(TokenKind::Condition);
    } else {
        // nom on 'condition'
        p.bump();
    }

    Some(m.complete(p, SyntaxKind::ConditionType))
}

fn range_type_tail(p: &mut Parser, lhs: CompletedMarker) -> Option<CompletedMarker> {
    debug_assert!(p.at(TokenKind::Range));

    let m = lhs.precede(p);
    p.bump();

    if p.at(TokenKind::Star) {
        let m = p.start();
        p.bump();
        m.complete(p, SyntaxKind::UnsizedBound);
    } else {
        // Just a regular range bound
        expr::expect_expr(p);
    }

    Some(m.complete(p, SyntaxKind::RangeType))
}
