//! Extensions to the generated nodes
use super::nodes::*;
use crate::ast::{helper, AstNode};
use crate::{
    AssignOp, BinaryOp, IoKind, LiteralParseError, LiteralValue, SyntaxElement, SyntaxKind,
    SyntaxToken, UnaryOp,
};

impl PPBinaryExpr {
    pub fn lhs(&self) -> Option<PPExpr> {
        helper::nodes(self.syntax()).next()
    }

    pub fn op_kind(&self) -> Option<BinaryOp> {
        let op = self.op_node()?;

        match op.kind() {
            SyntaxKind::KwOr => Some(BinaryOp::Or),
            SyntaxKind::Pipe => Some(BinaryOp::Or),
            SyntaxKind::KwAnd => Some(BinaryOp::And),
            SyntaxKind::Ampersand => Some(BinaryOp::And),
            _ => None,
        }
    }

    pub fn op_node(&self) -> Option<SyntaxElement> {
        self.syntax().children_with_tokens().nth(1)
    }

    pub fn rhs(&self) -> Option<PPExpr> {
        helper::nodes(self.syntax()).nth(1)
    }
}

impl PPUnaryExpr {
    pub fn op_kind(&self) -> Option<UnaryOp> {
        let op = self.op_token()?;

        match op.kind() {
            SyntaxKind::KwNot | SyntaxKind::Tilde => Some(UnaryOp::Not),
            _ => None,
        }
    }

    pub fn op_token(&self) -> Option<SyntaxToken> {
        self.syntax().children_with_tokens().next()?.into_token()
    }

    pub fn rhs(&self) -> Option<PPExpr> {
        helper::nodes(self.syntax()).next()
    }
}

impl BinaryExpr {
    pub fn lhs(&self) -> Option<Expr> {
        helper::nodes(self.syntax()).next()
    }

    pub fn op_kind(&self) -> Option<BinaryOp> {
        let op = self.op_node()?;

        match op.kind() {
            SyntaxKind::Imply => Some(BinaryOp::Imply),
            SyntaxKind::KwOr => Some(BinaryOp::Or),
            SyntaxKind::Pipe => Some(BinaryOp::Or),
            SyntaxKind::KwAnd => Some(BinaryOp::And),
            SyntaxKind::Ampersand => Some(BinaryOp::And),
            SyntaxKind::Less => Some(BinaryOp::Less),
            SyntaxKind::Greater => Some(BinaryOp::Greater),
            SyntaxKind::LessEqu => Some(BinaryOp::LessEq),
            SyntaxKind::GreaterEqu => Some(BinaryOp::GreaterEq),
            SyntaxKind::Equ => Some(BinaryOp::Equal),
            SyntaxKind::NotEq => Some(BinaryOp::NotEqual),
            SyntaxKind::KwIn => Some(BinaryOp::In),
            SyntaxKind::NotIn => Some(BinaryOp::NotIn),
            SyntaxKind::Plus => Some(BinaryOp::Add),
            SyntaxKind::Minus => Some(BinaryOp::Sub),
            SyntaxKind::KwXor => Some(BinaryOp::Xor),
            SyntaxKind::Star => Some(BinaryOp::Mul),
            SyntaxKind::Slash => Some(BinaryOp::RealDiv),
            SyntaxKind::KwDiv => Some(BinaryOp::Div),
            SyntaxKind::KwMod => Some(BinaryOp::Mod),
            SyntaxKind::KwRem => Some(BinaryOp::Rem),
            SyntaxKind::KwShl => Some(BinaryOp::Shl),
            SyntaxKind::KwShr => Some(BinaryOp::Shr),
            SyntaxKind::Exp => Some(BinaryOp::Exp),
            _ => None,
        }
    }

    pub fn op_node(&self) -> Option<SyntaxElement> {
        self.syntax().children_with_tokens().nth(1)
    }

    pub fn rhs(&self) -> Option<Expr> {
        helper::nodes(self.syntax()).nth(1)
    }
}

impl UnaryExpr {
    pub fn op_kind(&self) -> Option<UnaryOp> {
        let op = self.op_token()?;

        match op.kind() {
            SyntaxKind::KwNot | SyntaxKind::Tilde => Some(UnaryOp::Not),
            SyntaxKind::Plus => Some(UnaryOp::Identity),
            SyntaxKind::Minus => Some(UnaryOp::Negate),
            _ => None,
        }
    }

    pub fn op_token(&self) -> Option<SyntaxToken> {
        self.syntax().children_with_tokens().next()?.into_token()
    }

    pub fn rhs(&self) -> Option<Expr> {
        helper::nodes(self.syntax()).next()
    }
}

impl LiteralExpr {
    pub fn literal(&self) -> Option<(LiteralValue, Option<LiteralParseError>)> {
        let literal = self.syntax().first_token()?;

        match literal.kind() {
            SyntaxKind::IntLiteral => {
                // Basic integer, parsing with radix 10
                let text = literal.text();
                let value = lexical::parse::<u64, _>(text);

                let value = match value {
                    Ok(num) => (LiteralValue::Int(num), None),
                    Err(err) => {
                        let err = match err.code {
                            lexical::ErrorCode::Overflow => LiteralParseError::IntTooLarge,
                            _ => LiteralParseError::IntInvalid,
                        };
                        (LiteralValue::Int(0), Some(err))
                    }
                };

                Some(value)
            }
            SyntaxKind::RadixLiteral => {
                // Radix Integer parsing
                let text = literal.text();
                let (radix_slice, digits_slice) = text.split_at(text.find('#').unwrap());
                let digits_slice = &digits_slice[1..]; // skip over #

                let radix = lexical::parse::<u8, _>(radix_slice);
                let radix = match radix {
                    Ok(radix) if (2..=36).contains(&radix) => radix,
                    _ => {
                        // invalid radix value
                        return Some((
                            LiteralValue::Int(0),
                            Some(LiteralParseError::IntInvalidBase),
                        ));
                    }
                };

                // valid radix
                let value = lexical::parse_radix::<u64, _>(&digits_slice, radix);
                let value = match value {
                    Ok(num) => (LiteralValue::Int(num), None),
                    Err(err) => {
                        let err = match err.code {
                            lexical::ErrorCode::Overflow => LiteralParseError::IntRadixTooLarge,
                            lexical::ErrorCode::InvalidDigit => {
                                // Get the exact span of the invalid digit
                                // account for width of the radix & '#'
                                let start_slice = radix_slice.len() + 1 + err.index;
                                let end_slice = start_slice + 1;

                                // Chars are guarranteed to be in the ascii range
                                assert!(text.get(start_slice..end_slice).is_some());

                                LiteralParseError::IntRadixInvalidDigit(start_slice, end_slice)
                            }
                            lexical::ErrorCode::Empty => LiteralParseError::IntMissingRadix,
                            _ => LiteralParseError::IntInvalid,
                        };
                        (LiteralValue::Int(0), Some(err))
                    }
                };

                Some(value)
            }
            SyntaxKind::RealLiteral => {
                // Pretty simple real parsing
                let text = literal.text();

                // Check that the real literal is valid
                let value = lexical::parse_lossy::<f64, _>(text);
                let value = match value {
                    Ok(num) if num.is_infinite() => (
                        LiteralValue::Real(0.0),
                        Some(LiteralParseError::RealTooLarge),
                    ),
                    Ok(num) if num.is_nan() => (
                        LiteralValue::Real(0.0),
                        Some(LiteralParseError::RealInvalid),
                    ),
                    Err(err) => {
                        let err = match err.code {
                            lexical::ErrorCode::Overflow => LiteralParseError::RealTooLarge,
                            lexical::ErrorCode::Underflow => LiteralParseError::RealTooSmall,
                            lexical::ErrorCode::EmptyExponent => {
                                LiteralParseError::RealMissingExponent
                            }
                            // all other cases are protected by what is parsed, but still push out an error
                            _ => LiteralParseError::RealInvalid,
                        };
                        (LiteralValue::Real(0.0), Some(err))
                    }
                    Ok(num) => (LiteralValue::Real(num), None),
                };

                Some(value)
            }
            SyntaxKind::CharLiteral => todo!(),
            SyntaxKind::StringLiteral => todo!(),
            SyntaxKind::KwTrue => Some((LiteralValue::Boolean(true), None)),
            SyntaxKind::KwFalse => Some((LiteralValue::Boolean(false), None)),
            _ => None,
        }
    }
}

impl Reference {
    pub fn as_expr(self) -> Expr {
        match self {
            Reference::NameExpr(expr) => Expr::NameExpr(expr),
            Reference::SelfExpr(expr) => Expr::SelfExpr(expr),
            Reference::FieldExpr(expr) => Expr::FieldExpr(expr),
            Reference::DerefExpr(expr) => Expr::DerefExpr(expr),
            Reference::CheatExpr(expr) => Expr::CheatExpr(expr),
            Reference::NatCheatExpr(expr) => Expr::NatCheatExpr(expr),
            Reference::ArrowExpr(expr) => Expr::ArrowExpr(expr),
            Reference::IndirectExpr(expr) => Expr::IndirectExpr(expr),
            Reference::BitsExpr(expr) => Expr::BitsExpr(expr),
            Reference::CallExpr(expr) => Expr::CallExpr(expr),
        }
    }
}

impl IoCap {
    pub fn io_kind(&self) -> Option<IoKind> {
        match self.syntax().first_token()?.kind() {
            SyntaxKind::KwGet => Some(IoKind::Get),
            SyntaxKind::KwPut => Some(IoKind::Put),
            SyntaxKind::KwRead => Some(IoKind::Read),
            SyntaxKind::KwWrite => Some(IoKind::Write),
            SyntaxKind::KwSeek => Some(IoKind::Seek),
            SyntaxKind::KwMod => Some(IoKind::Mod),
            _ => None,
        }
    }

    pub fn io_kind_token(&self) -> Option<SyntaxToken> {
        self.syntax().first_child_or_token()?.into_token()
    }
}

impl AsnOp {
    pub fn asn_kind(&self) -> Option<AssignOp> {
        let op = self.asn_node()?;

        let kind = match op.kind() {
            SyntaxKind::Assign => AssignOp::None,
            SyntaxKind::Imply => AssignOp::Imply,
            SyntaxKind::KwOr => AssignOp::Or,
            SyntaxKind::Pipe => AssignOp::Or,
            SyntaxKind::KwAnd => AssignOp::And,
            SyntaxKind::Ampersand => AssignOp::And,
            SyntaxKind::Plus => AssignOp::Add,
            SyntaxKind::Minus => AssignOp::Sub,
            SyntaxKind::KwXor => AssignOp::Xor,
            SyntaxKind::Star => AssignOp::Mul,
            SyntaxKind::Slash => AssignOp::RealDiv,
            SyntaxKind::KwDiv => AssignOp::Div,
            SyntaxKind::KwMod => AssignOp::Mod,
            SyntaxKind::KwRem => AssignOp::Rem,
            SyntaxKind::KwShl => AssignOp::Shl,
            SyntaxKind::KwShr => AssignOp::Shr,
            SyntaxKind::Exp => AssignOp::Exp,
            _ => return None,
        };

        Some(kind)
    }

    pub fn asn_node(&self) -> Option<SyntaxElement> {
        self.syntax().children_with_tokens().next()
    }
}

impl TagStmt {
    pub fn tag_ref(&self) -> Option<Reference> {
        helper::nodes(self.syntax()).next()
    }

    pub fn tag_val(&self) -> Option<Expr> {
        helper::nodes(self.syntax()).nth(1)
    }
}

impl WaitStmt {
    pub fn wait_ref(&self) -> Option<Reference> {
        helper::nodes(self.syntax()).next()
    }

    pub fn wait_val(&self) -> Option<Expr> {
        helper::nodes(self.syntax()).nth(1)
    }
}

impl PrimType {
    // TODO: prim
}

impl RangeType {
    pub fn begin(&self) -> Option<Expr> {
        helper::nodes(self.syntax()).next()
    }

    pub fn end(&self) -> Option<EndBound> {
        helper::nodes(self.syntax()).nth(1)
    }
}
