//! Lowering into `Type` HIR nodes
use toc_hir::symbol::syms;
use toc_hir::{symbol, ty};
use toc_span::{HasSpanTable, Span, Spanned};
use toc_syntax::ast::{self, AstNode};

impl super::BodyLowering<'_, '_> {
    /// Lowers a required type. If not present, constructs a `Type::Missing` node in-place
    pub(super) fn lower_required_type(&mut self, ty: Option<ast::Type>) -> ty::TypeId {
        ty.and_then(|ty| self.lower_type(ty)).unwrap_or_else(|| {
            // Allocate a generic span
            let ty = ty::Type {
                kind: ty::TypeKind::Missing,
                span: self.ctx.library.span_table().dummy_span(),
            };
            self.ctx.library.intern_type(ty)
        })
    }

    /// Lowers a type.
    pub(super) fn lower_type(&mut self, ty: ast::Type) -> Option<ty::TypeId> {
        let span = self.ctx.mk_span(ty.syntax().text_range());

        let kind = match ty {
            ast::Type::PrimType(ty) => self.lower_prim_type(ty),
            ast::Type::NameType(ty) => self.lower_name_type(ty),
            ast::Type::RangeType(ty) => self.lower_constrained_type(ty),
            ast::Type::EnumType(ty) => self.lower_enum_type(ty),
            ast::Type::ArrayType(ty) => self.lower_array_type(ty),
            ast::Type::SetType(ty) => self.lower_set_type(ty),

            ast::Type::RecordType(_) => self.unsupported_ty(span),
            ast::Type::UnionType(_) => self.unsupported_ty(span),

            ast::Type::PointerType(ty) => self.lower_pointer_type(ty),
            ast::Type::FcnType(ty) => self.lower_fcn_type(ty),
            ast::Type::ProcType(ty) => self.lower_proc_type(ty),

            ast::Type::CollectionType(_) => self.unsupported_ty(span),
            ast::Type::ConditionType(_) => self.unsupported_ty(span),
        }?;

        let span = self.ctx.library.intern_span(span);
        let ty = ty::Type { kind, span };
        Some(self.ctx.library.intern_type(ty))
    }

    fn unsupported_ty(&mut self, span: Span) -> Option<ty::TypeKind> {
        self.ctx
            .messages
            .error("unsupported type", "this type is not handled yet", span);
        None
    }

    fn lower_prim_type(&mut self, ty: ast::PrimType) -> Option<ty::TypeKind> {
        let kind = match ty.prim()? {
            toc_syntax::PrimitiveKind::Int => ty::Primitive::Int,
            toc_syntax::PrimitiveKind::Int1 => ty::Primitive::Int1,
            toc_syntax::PrimitiveKind::Int2 => ty::Primitive::Int2,
            toc_syntax::PrimitiveKind::Int4 => ty::Primitive::Int4,
            toc_syntax::PrimitiveKind::Nat => ty::Primitive::Nat,
            toc_syntax::PrimitiveKind::Nat1 => ty::Primitive::Nat1,
            toc_syntax::PrimitiveKind::Nat2 => ty::Primitive::Nat2,
            toc_syntax::PrimitiveKind::Nat4 => ty::Primitive::Nat4,
            toc_syntax::PrimitiveKind::Real => ty::Primitive::Real,
            toc_syntax::PrimitiveKind::Real4 => ty::Primitive::Real4,
            toc_syntax::PrimitiveKind::Real8 => ty::Primitive::Real8,
            toc_syntax::PrimitiveKind::Boolean => ty::Primitive::Boolean,
            toc_syntax::PrimitiveKind::AddressInt => ty::Primitive::AddressInt,
            toc_syntax::PrimitiveKind::Char => ty::Primitive::Char,
            toc_syntax::PrimitiveKind::String => ty::Primitive::String,
            toc_syntax::PrimitiveKind::SizedChar(node) => {
                ty::Primitive::SizedChar(self.lower_seq_length(node))
            }
            toc_syntax::PrimitiveKind::SizedString(node) => {
                ty::Primitive::SizedString(self.lower_seq_length(node))
            }
        };

        Some(ty::TypeKind::Primitive(kind))
    }

    fn lower_seq_length(&mut self, node: Option<ast::SeqLength>) -> ty::SeqLength {
        match node {
            Some(node) if node.star_token().is_some() => ty::SeqLength::Any,
            seq_length => {
                let expr = seq_length.and_then(|node| node.expr());
                let body = self.lower_required_expr_body(expr);
                ty::SeqLength::Expr(body)
            }
        }
    }

    fn lower_name_type(&mut self, ty: ast::NameType) -> Option<ty::TypeKind> {
        let mut segments = vec![];
        let mut expr = ty.expr()?;

        loop {
            match expr {
                ast::Expr::NameExpr(expr) => {
                    // was pushed in reverse order, need to correct it
                    segments.reverse();

                    // at a simple alias
                    let name = expr.name()?.identifier_token()?;
                    let span = self.ctx.mk_span(name.text_range());
                    let def_id = self.ctx.use_sym(name.text().into(), span);

                    break Some(ty::TypeKind::Alias(ty::Alias {
                        base_def: Spanned::new(def_id, self.ctx.intern_range(name.text_range())),
                        segments,
                    }));
                }
                ast::Expr::FieldExpr(field) => {
                    let name = field.name()?.identifier_token()?;

                    segments.push(Spanned::new(
                        name.text().into(),
                        self.ctx.intern_range(name.text_range()),
                    ));

                    // keep walking up chain
                    expr = field.expr()?;
                }
                _ => {
                    let span = self.ctx.mk_span(expr.syntax().text_range());

                    // Not a valid named type
                    self.ctx.messages.error(
                        "invalid type",
                        "expressions can't be used as types",
                        span,
                    );
                    break None;
                }
            }
        }
    }

    fn lower_constrained_type(&mut self, ty: ast::RangeType) -> Option<ty::TypeKind> {
        let start = self.lower_required_expr_body(ty.start());
        let end = match ty.end() {
            Some(ast::EndBound::Expr(end)) => ty::ConstrainedEnd::Expr(self.lower_expr_body(end)),
            Some(ast::EndBound::UnsizedBound(bound)) => {
                let bound_span = self.ctx.intern_range(bound.syntax().text_range());

                // Get the closest `init` initializer to steal from
                let elem_count = if let Some(array) = ty
                    .syntax()
                    .parent()
                    .and_then(ast::RangeList::cast)
                    .and_then(|list| list.syntax().parent())
                    .and_then(ast::ArrayType::cast)
                {
                    if let Some(decl) = array.syntax().parent().and_then(ast::ConstVarDecl::cast) {
                        if let Some(ast::Expr::InitExpr(init)) = decl.init() {
                            // Count elems directly
                            let elem_count = init
                                .expr_list()
                                .map(|list| list.exprs().count())
                                .unwrap_or(0);

                            match u32::try_from(elem_count) {
                                Ok(count) => Some(count),
                                Err(_) => {
                                    // Does not fit within a u32
                                    // Even though we may support 64-bit ints in the future,
                                    // this is for reserving elements for an array, so capping
                                    // the elem count at `u32::MAX` seems reasonable
                                    //
                                    // This is different from the use case as constraining value
                                    // range, which uses explicit sized bounds.

                                    // We can't test this without lowering the limit
                                    self.ctx.messages.error(
                                        "too many elements in `init` initializer",
                                        format!(
                                            "`init` initializer has more than {limit} elements",
                                            limit = u32::MAX
                                        ),
                                        self.ctx.mk_span(init.syntax().text_range()),
                                    );

                                    // Saturate at u32::MAX
                                    Some(u32::MAX)
                                }
                            }
                        } else {
                            // Is a `ConstVar`, but the initializer isn't an `init`
                            let this_span = self.ctx.mk_span(ty.syntax().text_range());
                            let (err, err_span) = match decl.init() {
                                Some(expr) => {
                                    let err_span = self.ctx.mk_span(expr.syntax().text_range());
                                    ("not an `init` expression", err_span)
                                }
                                None => {
                                    let err_span = self.ctx.mk_span(decl.syntax().text_range());
                                    ("missing an initializer expression", err_span)
                                }
                            };

                            self.ctx
                                .messages
                                .error_detailed(
                                    "unsized range types require an `init` initializer",
                                    this_span,
                                )
                                .with_error(err, err_span)
                                .finish();

                            None
                        }
                    } else {
                        // Not inside a `ConstVar` type spec
                        let this_span = self.ctx.mk_span(ty.syntax().text_range());
                        self.ctx.messages.error(
                            "unsized range types require an `init` initializer",
                            "not inside a `const` or `var` declaration",
                            this_span,
                        );

                        None
                    }
                } else {
                    // Only allowed in array types
                    let this_span = self.ctx.mk_span(ty.syntax().text_range());
                    self.ctx.messages.error(
                        "cannot use unsized range type here",
                        "unsized range types can only be used in array ranges",
                        this_span,
                    );
                    None
                };

                ty::ConstrainedEnd::Unsized(Spanned::new(elem_count, bound_span))
            }
            // Treat as a missing end
            None => ty::ConstrainedEnd::Expr(self.lower_required_expr_body(None)),
        };

        // Dynamic end bound is only allowed if we're in a `var` decl, and part of the array ranges
        let allow_dyn = if let Some(cv_decl) = ty
            .syntax()
            .parent()
            .and_then(ast::RangeList::cast)
            .and_then(|ty| ty.syntax().parent())
            .and_then(ast::ArrayType::cast)
            .and_then(|ty| ty.syntax().parent())
            .and_then(ast::ConstVarDecl::cast)
        {
            cv_decl.var_token().is_some()
        } else {
            false
        };

        Some(ty::TypeKind::Constrained(ty::Constrained {
            start,
            end,
            allow_dyn,
        }))
    }

    fn lower_enum_type(&mut self, ty: ast::EnumType) -> Option<ty::TypeKind> {
        let span = self.ctx.intern_range(ty.syntax().text_range());
        let variants = ty
            .fields()
            .unwrap()
            .names()
            .filter_map(|name| {
                let span = self.ctx.intern_range(name.syntax().text_range());
                let name = name.identifier_token()?.text().into();
                let def_id = self
                    .ctx
                    .library
                    .add_def(name, span, symbol::SymbolKind::Declared);

                Some(def_id)
            })
            .collect();
        let def_id =
            self.ctx
                .library
                .add_def(type_decl_name(ty), span, symbol::SymbolKind::Declared);

        Some(ty::TypeKind::Enum(ty::Enum { def_id, variants }))
    }

    fn lower_array_type(&mut self, ty: ast::ArrayType) -> Option<ty::TypeKind> {
        let range_list = ty.range_list().unwrap();

        // If there's one init-sized bound, then that must be the only one
        let init_bound = range_list.ranges().find_map(|range| {
            if let ast::Type::RangeType(range) = range {
                let is_init_sized = range
                    .end()
                    .map(|end| matches!(end, ast::EndBound::UnsizedBound(_)))
                    .is_some();

                is_init_sized.then(|| range)
            } else {
                None
            }
        });

        let ranges = if let Some(init_sized) = init_bound {
            // Must be the only bound
            let (mut ranges_before, ranges_after) =
                range_list.ranges().partition::<Vec<_>, _>(|range| {
                    range
                        .syntax()
                        .text_range()
                        .ordering(init_sized.syntax().text_range())
                        .is_le()
                });

            // Remove the duplicate init-sized range
            {
                let before = ranges_before.pop();
                assert!(matches!(before, Some(ast::Type::RangeType(other)) if other == init_sized));
            }

            if !ranges_before.is_empty() || !ranges_after.is_empty() {
                // Extra ranges specified
                let before_span = ranges_before
                    .first()
                    .zip(ranges_before.last())
                    .map(|(a, b)| {
                        self.ctx
                            .mk_span(a.syntax().text_range().cover(b.syntax().text_range()))
                    });
                let after_span = ranges_after.first().zip(ranges_after.last()).map(|(a, b)| {
                    self.ctx
                        .mk_span(a.syntax().text_range().cover(b.syntax().text_range()))
                });
                let init_span = self.ctx.mk_span(init_sized.syntax().text_range());

                let mut builder = self
                    .ctx
                    .messages
                    .error_detailed("extra ranges specified", init_span);
                if let Some(before_span) = before_span {
                    builder = builder.with_error("extra ranges before", before_span);
                }
                if let Some(after_span) = after_span {
                    builder = builder.with_error("extra ranges after", after_span);
                }
                builder
                    .with_note("this must be the only range present", init_span)
                    .with_info("`init`-sized arrays must have this range be the only range present")
                    .finish();
            }

            // Let this be the only bound
            vec![self.lower_required_type(Some(ast::Type::RangeType(init_sized)))]
        } else {
            // Lower all of the bounds
            range_list
                .ranges()
                .map(|ty| self.lower_required_type(Some(ty)))
                .collect()
        };

        let is_flexible = ty.flexible_token().is_some();
        let allow_dyn_size = ty
            .syntax()
            .parent()
            .and_then(ast::ConstVarDecl::cast)
            .map(|cv_decl| cv_decl.var_token().is_some())
            .unwrap_or(false);

        let sizing = if is_flexible {
            ty::ArraySize::Flexible
        } else if allow_dyn_size {
            ty::ArraySize::MaybeDyn
        } else {
            ty::ArraySize::Static
        };
        let elem_ty = self.lower_required_type(ty.elem_ty());

        Some(ty::TypeKind::Array(ty::Array {
            sizing,
            ranges,
            elem_ty,
        }))
    }

    fn lower_set_type(&mut self, ty: ast::SetType) -> Option<ty::TypeKind> {
        let span = self.ctx.intern_range(ty.syntax().text_range());
        let elem = self.lower_required_type(ty.elem_ty());
        let def_id =
            self.ctx
                .library
                .add_def(type_decl_name(ty), span, symbol::SymbolKind::Declared);

        Some(ty::TypeKind::Set(ty::Set {
            def_id,
            elem_ty: elem,
        }))
    }

    fn lower_pointer_type(&mut self, ty: ast::PointerType) -> Option<ty::TypeKind> {
        let is_checked = ty
            .is_checked()
            .map(|checked| checked.unchecked_token().is_none())
            .unwrap_or(true);
        let ty = self.lower_required_type(ty.to_ty());

        let checked = if is_checked {
            ty::Checked::Checked
        } else {
            ty::Checked::Unchecked
        };

        Some(ty::TypeKind::Pointer(ty::Pointer { checked, ty }))
    }

    fn lower_proc_type(&mut self, ty: ast::ProcType) -> Option<ty::TypeKind> {
        let param_list = self
            .lower_formals_spec(ty.params())
            .map(|params| params.tys);
        let return_ty = {
            let span = self.ctx.intern_range(ty.syntax().text_range());
            self.ctx.library.intern_type(ty::Type {
                kind: ty::TypeKind::Void,
                span,
            })
        };

        Some(ty::TypeKind::Subprogram(ty::Subprogram {
            kind: symbol::SubprogramKind::Procedure,
            param_list,
            result_ty: return_ty,
        }))
    }

    fn lower_fcn_type(&mut self, ty: ast::FcnType) -> Option<ty::TypeKind> {
        let param_list = self
            .lower_formals_spec(ty.params())
            .map(|params| params.tys);
        let return_ty = self.lower_required_type(ty.ty());

        Some(ty::TypeKind::Subprogram(ty::Subprogram {
            kind: symbol::SubprogramKind::Function,
            param_list,
            result_ty: return_ty,
        }))
    }
}

/// Gets the name from the enclosing `type` decl, or the [`Anonymous`](syms::Anonymous) symbol
fn type_decl_name(ty: impl ast::AstNode) -> symbol::Symbol {
    ty.syntax()
        .parent()
        .and_then(ast::TypeDecl::cast)
        .and_then(|node| node.decl_name())
        .map_or(*syms::Anonymous, |name| {
            name.identifier_token().unwrap().text().into()
        })
}
