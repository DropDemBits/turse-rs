//! Extensions to the generated nodes
use super::nodes::*;
use crate::ast::{helper, AstNode};
use crate::{AssignOp, BinaryOp, IoKind, SyntaxElement, SyntaxKind, SyntaxToken, UnaryOp};

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
    // TODO: literal
}

impl IoCap {
    pub fn io_kind(&self) -> Option<IoKind> {
        match self.syntax().kind() {
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
        self.syntax().children_with_tokens().nth(1)
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
