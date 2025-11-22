//! Pretty prints types for both debugging and displaying errors.

use std::fmt;

use toc_hir_def::{Mutability, body, expr};

use crate::{
    Db,
    infer::{self},
    ty::{IntSize, NatSize, RealSize, TyKind},
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ascription {
    // Byte position to insert at.
    pub insert_at: usize,
    // What to insert.
    pub ascription: String,
}

pub fn render_ascriptions<'db>(
    db: &'db dyn Db,
    body: body::Body<'db>,
    infer: infer::body::BodyInfer<'db>,
) -> Vec<Ascription> {
    let store = body.contents(db);
    let spans = body.spans(db);
    let mut ascriptions = vec![];

    for (expr, ty) in infer.expr_tys(db) {
        let span = spans.expr_span(*expr).absolute_text_range();
        let needs_parens = match store.expr(*expr) {
            expr::Expr::Missing => continue,
            expr::Expr::Literal(_)
            | expr::Expr::Init(_)
            | expr::Expr::All
            | expr::Expr::Name(_)
            | expr::Expr::Field(_)
            | expr::Expr::Call(_) => false,
            _ => true,
        };

        if needs_parens {
            ascriptions.push(Ascription {
                insert_at: span.start().into(),
                ascription: String::from("("),
            });
            ascriptions.push(Ascription {
                insert_at: span.end().into(),
                ascription: String::from(")"),
            });
        }

        use fmt::Write as _;
        let mut out = String::new();
        write!(&mut out, ".as[").unwrap();
        render_ty(db, ty.kind(db), true, &mut out).unwrap();
        write!(&mut out, "]").unwrap();

        ascriptions.push(Ascription {
            insert_at: span.end().into(),
            ascription: out,
        });
    }

    for (local, ty) in infer.local_tys(db) {
        let span = spans.local_span(*local).absolute_text_range();

        use fmt::Write as _;
        let mut out = String::new();
        write!(&mut out, ": ").unwrap();
        render_ty(db, ty.kind(db), true, &mut out).unwrap();

        ascriptions.push(Ascription {
            insert_at: span.end().into(),
            ascription: out,
        });
    }

    ascriptions
}

pub fn render_ty<'db>(
    db: &'db dyn Db,
    ty_kind: &'db TyKind,
    alternate: bool,
    out: &mut impl fmt::Write,
) -> fmt::Result {
    match ty_kind {
        TyKind::Error => write!(out, "<_>"),
        TyKind::FlexVar(var) => match *var {},
        TyKind::Place(ty_kind, mutability) => {
            if alternate {
                // Write out the full place type
                write!(out, "place[")?;
                render_ty(db, ty_kind, alternate, out)?;
                write!(out, ",")?;
                match mutability {
                    Mutability::Const => write!(out, "const")?,
                    Mutability::Var => write!(out, "var")?,
                }
                write!(out, "]")?;
                Ok(())
            } else {
                render_ty(db, ty_kind, alternate, out)
            }
        }
        TyKind::Boolean => write!(out, "boolean"),
        TyKind::Int(IntSize::Int1) => write!(out, "int1"),
        TyKind::Int(IntSize::Int2) => write!(out, "int2"),
        TyKind::Int(IntSize::Int4) => write!(out, "int4"),
        TyKind::Int(IntSize::Int) => write!(out, "int"),
        TyKind::Nat(NatSize::Nat1) => write!(out, "nat1"),
        TyKind::Nat(NatSize::Nat2) => write!(out, "nat2"),
        TyKind::Nat(NatSize::Nat4) => write!(out, "nat4"),
        TyKind::Nat(NatSize::Nat) => write!(out, "nat"),
        TyKind::Nat(NatSize::AddressInt) => write!(out, "addressint"),
        TyKind::Real(RealSize::Real4) => write!(out, "real4"),
        TyKind::Real(RealSize::Real8) => write!(out, "real8"),
        TyKind::Real(RealSize::Real) => write!(out, "real"),
        TyKind::Integer => write!(out, "{{integer}}"),
        TyKind::Number => write!(out, "{{number}}"),
        TyKind::Char => write!(out, "char"),
        TyKind::String => write!(out, "string"),
    }
}
