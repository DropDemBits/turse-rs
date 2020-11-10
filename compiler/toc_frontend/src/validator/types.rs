//! Validator fragment, resolves all type specifications
use super::Validator;
use toc_ast::ast;
use toc_ast::ast::expr::{Expr, ExprKind};
use toc_ast::ast::ident::RefKind;
use toc_ast::ast::types::{SeqSize, Type as TypeNode};
use toc_ast::ast::VisitorMut;
use toc_ast::types::{self, ParamInfo, PrimitiveType, SequenceSize, Type, TypeRef, TypeTable};
use toc_ast::value;

impl Validator {
    // --- Type Resolvers --- //

    pub(super) fn resolve_type_char_seq(
        &mut self,
        type_ref: &mut Option<TypeRef>,
        prim_type: PrimitiveType,
        size: &mut SeqSize,
    ) {
        // Build type
        match size {
            SeqSize::Any => {
                // `Any` is 0 size
                let prim_ref = match prim_type {
                    PrimitiveType::Char => PrimitiveType::CharN(SequenceSize::Size(0)),
                    PrimitiveType::String_ => PrimitiveType::StringN(SequenceSize::Size(0)),
                    _ => unreachable!("invalid primitive type"),
                };

                *type_ref = Some(TypeRef::Primitive(prim_ref))
            }
            SeqSize::Sized(expr) => {
                // Compute size
                self.visit_expr(expr);
                let computed_size = self.eval_expr(expr);

                if let Ok(computed_size) = computed_size {
                    let computed_size = match computed_size {
                        Some(value::Value::NatValue(v)) => Some(v), // Direct correspondence
                        Some(value::Value::IntValue(v)) => {
                            if v.is_negative() {
                                // Negative length is invalid
                                self.context.borrow_mut().reporter.report_error(
                                    expr.get_span(),
                                    format_args!("Expression results in a negative length"),
                                );
                                None
                            } else {
                                Some(v as u64)
                            }
                        }
                        Some(_) => {
                            // Wrong length type
                            self.context.borrow_mut().reporter.report_error(
                                expr.get_span(),
                                format_args!("Wrong type for a length specifier"),
                            );
                            None
                        }
                        None => {
                            // Not a compile-time expression!
                            self.context.borrow_mut().reporter.report_error(
                                &expr.get_span(),
                                format_args!("Length specifier is not a compile-time expression"),
                            );
                            None
                        }
                    };

                    let final_ref = computed_size.and_then(|computed_size| {
                        // Check if the size is within the correct range
                        if computed_size == 0 {
                            // Evaluates to zero, invalid
                            self.context.borrow_mut().reporter.report_error(
                                &expr.get_span(),
                                format_args!("Expression results in a length of 0"),
                            );

                            None
                        } else if computed_size as usize >= types::MAX_STRING_SIZE {
                            // Greater than max length, never valid
                            self.context.borrow_mut().reporter.report_error(
                                &expr.get_span(),
                                format_args!(
                                    "'{}' is larger than or equal to the maximum length of {}",
                                    computed_size,
                                    types::MAX_STRING_SIZE
                                ),
                            );

                            None
                        } else {
                            // Return the correct size type
                            let prim_ref = match prim_type {
                                PrimitiveType::Char => {
                                    PrimitiveType::CharN(SequenceSize::Size(computed_size as usize))
                                }
                                PrimitiveType::String_ => PrimitiveType::StringN(
                                    SequenceSize::Size(computed_size as usize),
                                ),
                                _ => unreachable!("invalid primitive type"),
                            };

                            Some(TypeRef::Primitive(prim_ref))
                        }
                    });

                    // Defer to base type in an error
                    *type_ref = Some(final_ref.unwrap_or_else(|| TypeRef::Primitive(prim_type)))
                } else {
                    // Defer to base type in an eval error
                    *type_ref = Some(TypeRef::Primitive(prim_type));
                }
            }
        }
    }

    pub(super) fn resolve_type_array(
        &mut self,
        type_ref: &mut Option<TypeRef>,
        ranges: &mut Vec<TypeNode>,
        element_type: &mut Box<TypeNode>,
        is_flexible: bool,
        is_init_sized: bool,
        allow_dyn_array: bool,
    ) {
        // Collect range type refs
        let ranges = ranges
            .iter_mut()
            .map(|range| {
                if let ast::types::TypeKind::Range { start, end } = &mut range.kind {
                    // Only allow dynamic end bound if this is a dyn array
                    self.resolve_type_range(
                        &mut range.type_ref,
                        start,
                        end,
                        &range.span,
                        allow_dyn_array,
                    )
                } else {
                    self.visit_type(range);
                }

                // Not required to be compile-time, unless we are in a compile-time context
                let mut range_ref = types::dealias_ref(range.type_ref(), &self.type_table);

                if !is_flexible && !is_init_sized {
                    // If the following holds true
                    // - The index type is a range,
                    // - This is an explict sized array (i.e. not `flexible` nor `init` sized)
                    // Check if it is a not a zero sized range
                    if let Some(Type::Range { end, size, .. }) =
                        self.type_table.type_from_ref(&range_ref)
                    {
                        // Not being `flexible` nor `init`-sized does not guarrantee that `end`
                        // is an `EndBound::Constant` (e.g. something hidden behind an alias)
                        if !matches!(end, types::RangeBound::Unknown) {
                            if *size == Some(0) {
                                // Zero sized ranges aren't allowed in compile-time array types
                                self.context.borrow_mut().reporter.report_error(
                                    &range.span,
                                    format_args!("Range bounds creates a zero-sized range"),
                                );
                                range_ref = TypeRef::TypeError;
                            }
                        }
                    }
                }

                range_ref
            })
            .collect();

        // Resolve the element type
        self.visit_type(element_type);
        let element_ref = *element_type.type_ref();

        // Make the array type
        let ty_array = self.type_table.declare_type(Type::Array {
            ranges,
            element_type: element_ref,
            is_flexible,
            is_init_sized,
        });

        *type_ref = Some(TypeRef::Named(ty_array))
    }

    pub(super) fn resolve_type_function(
        &mut self,
        type_ref: &mut Option<TypeRef>,
        params: &mut Option<Vec<(Box<TypeNode>, ParamInfo)>>,
        result: &mut Option<Box<TypeNode>>,
    ) {
        // Make type refs for each of the parameters
        let params = params.as_mut().map(|params| {
            let params = params
                .iter_mut()
                .map(|(ty, param)| {
                    self.visit_type(ty);
                    (*ty.type_ref(), param.clone())
                })
                .collect();

            params
        });

        // Resolve the result type
        let result = result.as_mut().map(|result| {
            self.visit_type(result);
            *result.type_ref()
        });

        // Make type
        let ty_fcn = self
            .type_table
            .declare_type(Type::Function { params, result });

        *type_ref = Some(TypeRef::Named(ty_fcn))
    }

    pub(super) fn resolve_type_range(
        &mut self,
        type_ref: &mut Option<TypeRef>,
        start: &mut Box<Expr>,
        end: &mut SeqSize,
        span: &toc_core::Location,
        allow_dynamic_end_bound: bool,
    ) {
        // Visit the bound expressions
        let start_eval = {
            self.visit_expr(start);
            self.eval_expr(start).ok().flatten()
        };

        let end_eval = if let SeqSize::Sized(end) = end {
            self.visit_expr(end);
            self.eval_expr(end).ok().flatten()
        } else {
            None
        };

        // Apply the folded values
        super::replace_with_folded(start, start_eval);

        if let SeqSize::Sized(end) = end {
            super::replace_with_folded(end, end_eval);
        }

        if !start.is_compile_eval() {
            // The start range must be a compile-time expression

            // Report error if the bound is not an empty
            // Otherwise, error is already reported at the end bound's location
            if !matches!(start.kind, ExprKind::Error) {
                // Span over the start bound
                self.context.borrow_mut().reporter.report_error(
                    start.get_span(),
                    format_args!("Start bound must be a compile-time expression"),
                );
            }

            // Produce a type error as this is not a valid expression
            *type_ref = Some(TypeRef::TypeError);
            return;
        }

        if !allow_dynamic_end_bound {
            // All type info must be known at compile-time

            // Validate that the range type ref references a range that
            // has the end bound as a compile-time expression
            // Don't need to worry about checking * (checked by the parser)
            if let SeqSize::Sized(end) = end {
                if !end.is_compile_eval() {
                    // Right-hand side is not a compile-time expression

                    // Report the error if it's not an empty
                    // Otherwise, error is already reported at the end bound's location
                    if !matches!(end.kind, ExprKind::Error) {
                        // Span over the end bound
                        self.context.borrow_mut().reporter.report_error(
                            end.get_span(),
                            format_args!("End bound must be a compile-time expression"),
                        );
                    }

                    // Range is not a valid type
                    *type_ref = Some(TypeRef::TypeError);
                    return;
                }
            }
        }

        // Bounds are guarranteed to be something, safe to directly get_span
        let range_span = span;

        // Try to derive a base copy from the given types
        let start_type = start.get_eval_type();
        let end_type = if let SeqSize::Sized(end) = end {
            end.get_eval_type()
        } else {
            // No specified end range, use the start type
            start_type
        };

        if !types::is_equivalent_to(&start_type, &end_type, &self.type_table) {
            // Range eval types do not match
            self.context.borrow_mut().reporter.report_error(&range_span, format_args!("Range bounds must be both integers, characters, booleans, or elements from the same enumeration"));

            *type_ref = Some(TypeRef::TypeError);
            return;
        } else if (types::is_char_seq_type(&start_type)
            && types::get_sized_len(&start_type).unwrap_or(0) != 1)
            || (types::is_char_seq_type(&end_type)
                && types::get_sized_len(&end_type).unwrap_or(0) != 1)
        {
            // Range eval types are the wrong types
            self.context.borrow_mut().reporter.report_error(&range_span, format_args!("Range bounds must be both integers, characters, booleans, or elements from the same enumeration"));

            *type_ref = Some(TypeRef::TypeError);
            return;
        }

        // Check if the start and end bounds form a positive range size
        let mut size = None;
        if let SeqSize::Sized(end) = end {
            if end.is_compile_eval() && start.is_compile_eval() {
                let range_size = get_range_size(&start, &end, &self.type_table);

                if let Err(size_err) = range_size {
                    match size_err {
                        RangeSizeError::Overflow => {
                            // Cap the size to usize max
                            self.context.borrow_mut().reporter.report_warning(
                                &range_span,
                                format_args!(
                                    "Range bound size exceeds the maximum representable size"
                                ),
                            );

                            size = Some(usize::MAX);
                        }
                        RangeSizeError::NegativeSize => {
                            self.context.borrow_mut().reporter.report_error(
                                &range_span,
                                format_args!("Range bounds creates a negative-sized range"),
                            );

                            *type_ref = Some(TypeRef::TypeError);
                            return;
                        }
                        RangeSizeError::WrongTypes => {
                            // Wrong types, handle here!
                            self.context.borrow_mut().reporter.report_error(&range_span,format_args!("Range bounds must both be integers, characters, booleans, or elements from the same enumeration"));

                            *type_ref = Some(TypeRef::TypeError);
                            return;
                        }
                    }
                } else {
                    // Update range size
                    size = Some(range_size.ok().unwrap());
                }
            } else {
                // Not compile time, keep as dynamic
                size = None
            }
        }

        // Update the base type
        // Use `end_type` as it may use a larger base type than `start_type`
        let mut base_type = end_type;

        // If the base type is an enum field, take the associated enum type
        if let Some(Type::EnumField { enum_type, .. }) = self.type_table.type_from_ref(&base_type) {
            base_type = *enum_type;
        }

        if types::is_intnat(&base_type) {
            // Force into int from intnat
            base_type = TypeRef::Primitive(PrimitiveType::Int);
        }

        // Make range type (bounds are made by flattening through ords)
        let start = value::apply_ord(&start, &self.type_table)
            .ok()
            .map_or(types::RangeBound::Unknown, |v| v.into());
        let end = if let SeqSize::Sized(end) = end {
            value::apply_ord(&end, &self.type_table)
                .ok()
                .map_or(types::RangeBound::Unknown, |v| v.into())
        } else {
            types::RangeBound::Unknown
        };

        let ty_range = self.type_table.declare_type(Type::Range {
            start,
            end,
            base_type,
            size,
        });

        *type_ref = Some(TypeRef::Named(ty_range));
    }

    pub(super) fn resolve_type_reference(
        &mut self,
        type_ref: &mut Option<TypeRef>,
        expr: &mut Box<Expr>,
        allow_forward_refs: bool,
    ) {
        // Reference will produce a reference to the associated type_spec
        // If there is no reference to a type, a TypeError is produced

        // Evaluate expression
        self.visit_expr(expr);

        // Error reporting purposes
        let reference_locate;
        let ref_type;

        // Ensure that the top-most expression resolves to a type
        match &expr.kind {
            ExprKind::Dot {
                left,
                field: (field, location),
                ..
            } => {
                if field.ref_kind != RefKind::Type {
                    // Should always either be a dot, or a reference
                    // Otherwise, the expr is an error
                    let member_ident = self.get_reference_ident(left);

                    if let Some((base_name, ..)) = member_ident {
                        self.context.borrow_mut().reporter.report_error(
                            &location,
                            format_args!(
                                "Field '{}' of '{}' does not refer to a type",
                                field.name, base_name
                            ),
                        );
                    } else {
                        // Report error, but not the name
                        self.context.borrow_mut().reporter.report_error(
                            &location,
                            format_args!("Field '{}' does not refer to a type", field.name),
                        );
                    }

                    // Produce a type error
                    *type_ref = Some(TypeRef::TypeError);
                    return;
                }

                reference_locate = *location;
                ref_type = field.type_spec;
            }
            ExprKind::Reference { ident, .. } => {
                let info = self.unit_scope.get_ident_info(&ident.id);

                if info.ref_kind != RefKind::Type {
                    self.context.borrow_mut().reporter.report_error(
                        &ident.location,
                        format_args!("'{}' does not refer to a type", info.name),
                    );

                    // Produce a type error
                    *type_ref = Some(TypeRef::TypeError);
                    return;
                }

                reference_locate = ident.location;
                ref_type = info.type_spec;
            }
            _ => {
                // No other expressions allowed, produce a type error
                *type_ref = Some(TypeRef::TypeError);
                return;
            }
        }

        // De-alias ref type
        let ref_type = types::dealias_ref(&ref_type, &self.type_table);

        // Check if the eval type leads to a forward
        if let Some(Type::Forward { is_resolved }) = self.type_table.type_from_ref(&ref_type) {
            if !*is_resolved {
                // The type is not resolved at all, replace with TypeError
                self.context.borrow_mut().reporter.report_error(
                    &reference_locate,
                    format_args!("Type reference is not resolved in the current unit"),
                );

                *type_ref = Some(TypeRef::TypeError);
                return;
            } else if !allow_forward_refs {
                // The type ref is required to be resolved at this point, replace with TypeError
                self.context.borrow_mut().reporter.report_error(
                    &reference_locate,
                    format_args!("Type reference is required to be resolved at this point"),
                );

                *type_ref = Some(TypeRef::TypeError);
                return;
            }
        }

        // No type to build, chains off of previously built types
        *type_ref = Some(ref_type);
    }

    pub(super) fn resolve_type_set(
        &mut self,
        type_ref: &mut Option<TypeRef>,
        index: &mut Box<TypeNode>,
    ) {
        // Visit index type
        self.visit_type(index);

        // Doesn't matter if the range is a type error or not, will be
        // ignored during equivalence checking
        let mut index_ref = types::dealias_ref(index.type_ref(), &self.type_table);

        // Check that the index reference is actually an index type and not a reference to a non-index type
        // Other cases are handled by the parser
        if !types::is_error(&index_ref) && !types::is_index_type(&index_ref, &self.type_table) {
            // Not a real index type, change it to point to a type error
            index_ref = TypeRef::TypeError;

            // Report the error
            self.context.borrow_mut().reporter.report_error(
                &index.span,
                format_args!("Set index is not a range, char, boolean, or enumerated type"),
            );
        }

        // If the index type is a range, check if it is a not a zero sized range
        if let Some(Type::Range { size, .. }) = self.type_table.type_from_ref(&index_ref) {
            // Compile-time enforcement guarrantees that end is a some
            if *size == Some(0) {
                // Zero sized ranges aren't allowed in set types
                self.context.borrow_mut().reporter.report_error(
                    &index.span,
                    format_args!("Range bounds creates a zero-sized range"),
                );

                index_ref = TypeRef::TypeError;
            }
        }

        // Make the set type
        let set = self.type_table.declare_type(Type::Set { range: index_ref });

        *type_ref = Some(TypeRef::Named(set))
    }

    pub(super) fn resolve_type_forward(&mut self, _type_ref: &mut Option<TypeRef>) {
        // do nothing
    }

    pub(super) fn resolve_type_pointer(
        &mut self,
        type_ref: &mut Option<TypeRef>,
        is_unchecked: bool,
        to: &mut Box<TypeNode>,
    ) {
        // Visit the 'to' type (allow forward references)
        if let ast::types::TypeKind::Reference { ref_expr } = &mut to.kind {
            self.resolve_type_reference(&mut to.type_ref, ref_expr, true);
        } else {
            self.visit_type(to);
        }

        // Construct pointer type info
        let ptr = self.type_table.declare_type(Type::Pointer {
            to: *to.type_ref(),
            is_unchecked,
        });

        *type_ref = Some(TypeRef::Named(ptr));
    }

    pub(super) fn resolve_type_enum(
        &mut self,
        type_ref: &mut Option<TypeRef>,
        fields: &Vec<String>,
    ) {
        // Make enum stuff
        let parent_enum = self
            .type_table
            .declare_type(Type::Forward { is_resolved: true });

        // Create & insert fields
        let mut field_map = std::collections::HashMap::new();

        fields
            .iter()
            .enumerate()
            .map(|(ordinal, name)| {
                let field_id = self.type_table.declare_type(Type::EnumField {
                    enum_type: TypeRef::Named(parent_enum),
                    ordinal,
                });

                (name, TypeRef::Named(field_id))
            })
            .for_each(|(name, field)| {
                field_map.insert(name.clone(), field);
            });

        // Make the actual enum type
        self.type_table
            .replace_type(parent_enum, Type::Enum { fields: field_map });

        *type_ref = Some(TypeRef::Named(parent_enum));
    }
}

/// Errors when computing the range size
pub enum RangeSizeError {
    /// Range size has overflowed
    Overflow,
    /// Range size is negative
    NegativeSize,
    /// Either range bound is the wrong type
    WrongTypes,
}

/// Gets the size of the given range.
///
/// # Return Values
/// Returns a `Ok(u64)` if the range is a valid, otherwise an appropriate error.
pub(super) fn get_range_size(
    start_bound: &Expr,
    end_bound: &Expr,
    type_table: &TypeTable,
) -> Result<usize, RangeSizeError> {
    use std::convert::TryInto;

    // Converts value into i128's
    fn to_i128(value: value::Value) -> Result<i128, RangeSizeError> {
        match value {
            value::Value::IntValue(v) => Ok(i128::from(v)),
            value::Value::NatValue(v) => Ok(i128::from(v)),
            _ => Err(RangeSizeError::WrongTypes),
        }
    }

    // Apply 'ord' to convert into appropriate ranges
    let start_bound = value::apply_ord(start_bound, type_table)
        .map_or_else(|_| Err(RangeSizeError::WrongTypes), to_i128)?;
    let end_bound = value::apply_ord(end_bound, type_table)
        .map_or_else(|_| Err(RangeSizeError::WrongTypes), to_i128)?;

    // Compute range size (inclusive of end bound)
    let range_size = (end_bound + 1) - start_bound;

    if range_size.is_negative() {
        Err(RangeSizeError::NegativeSize)
    } else {
        // Failliable conversion down into usize
        let range_size: usize = range_size
            .try_into()
            .map_err(|_| RangeSizeError::Overflow)?;
        Ok(range_size)
    }
}
