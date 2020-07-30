//! Validator fragment, resolves all type specifications
use super::{ResolveContext, ResolveResult, Validator};
use crate::compiler::ast::{Expr, VisitorMut};
use crate::compiler::frontend::token::TokenType;
use crate::compiler::types::{
    self, ParamDef, PrimitiveType, SequenceSize, Type, TypeRef, TypeTable,
};
use crate::compiler::value::{self, ValueApplyError};
use std::convert::TryFrom;

impl Validator {
    // --- Type Resolvers --- //

    fn resolve_char_seq_size(
        &mut self,
        base_ref: TypeRef,
        resolving_context: ResolveContext,
    ) -> TypeRef {
        match base_ref {
            TypeRef::Primitive(PrimitiveType::CharN(seq_size))
            | TypeRef::Primitive(PrimitiveType::StringN(seq_size)) => {
                if let SequenceSize::CompileExpr(type_id) = seq_size {
                    // Resolve the type id first
                    let resolved_ref =
                        self.resolve_type(TypeRef::Named(type_id), resolving_context);

                    if types::is_error(&resolved_ref) {
                        // Not a valid size
                        // Convert to the corresponding base type (matching Turing behaviour)
                        return types::get_char_seq_base_type(base_ref);
                    }

                    let expr_id = types::get_type_id(&resolved_ref).unwrap();

                    // Grab the expression id, verify it's a literal, as well as being the correct type (int/nat/intnat)
                    if let Type::SizeExpr { expr } = self.type_table.get_type(expr_id) {
                        let computed_size = if let Expr::Literal { value, .. } = &**expr {
                            match value.token_type {
                                TokenType::NatLiteral(len) => Some(len), // Direct correspondence
                                TokenType::IntLiteral(len) => {
                                    if len < 0 {
                                        // Negative length is invalid
                                        self.reporter.report_error(
                                            &value.location,
                                            format_args!(
                                                "Compile-time string length specifier is negative"
                                            ),
                                        );
                                        None
                                    } else {
                                        Some(len as u64)
                                    }
                                }
                                _ => {
                                    // Wrong length type
                                    self.reporter.report_error(
                                        &value.location,
                                        format_args!("Wrong type for a string length specifier"),
                                    );
                                    None
                                }
                            }
                        } else {
                            // Not a compile-time expression!
                            self.reporter.report_error(
                                &expr.get_span(),
                                format_args!(
                                    "String length specifier is not a compile-time expression"
                                ),
                            );
                            None
                        };

                        if let Some(size) = computed_size {
                            // Check if the size is within the correct range
                            if size == 0 {
                                // Is zero, not directly specified by star
                                self.reporter.report_error(
                                    &expr.get_span(),
                                    format_args!("Invalid maximum string length of '0'"),
                                );

                                return types::get_char_seq_base_type(base_ref);
                            } else if size as usize >= types::MAX_STRING_SIZE {
                                // Greater than max length, never valid
                                self.reporter.report_error(
                                    &expr.get_span(),
                                    format_args!(
                                        "'{}' is larger than or equal to the maximum string length of '{}' (after including the end byte)",
                                        size,
                                        types::MAX_STRING_SIZE
                                    ),
                                );

                                return types::get_char_seq_base_type(base_ref);
                            } else {
                                // Return the correct size type
                                if types::is_charn(&base_ref) {
                                    return TypeRef::Primitive(PrimitiveType::CharN(
                                        SequenceSize::Size(size as usize),
                                    ));
                                } else {
                                    return TypeRef::Primitive(PrimitiveType::StringN(
                                        SequenceSize::Size(size as usize),
                                    ));
                                }
                            }
                        } else {
                            // Not a valid size, error reported previously
                            return types::get_char_seq_base_type(base_ref);
                        }
                    } else {
                        // There should always be a SizeExpr here, unless some bad input was fed through a pre-compiled library
                        unreachable!()
                    }
                } else {
                    // Already resolved, don't need to do anything
                    return base_ref;
                }
            }
            _ => unreachable!(), // Invalid primitive type or type ref for length resolving, called on wrong path?
        }
    }

    /// Resolves the given type, validating that the type is a valid type
    /// Returns the resolved typedef
    pub(super) fn resolve_type(
        &mut self,
        base_ref: TypeRef,
        resolving_context: ResolveContext,
    ) -> TypeRef {
        if types::is_sized_char_seq_type(&base_ref) {
            // Try and resolve the size of the char(n)
            return self.resolve_char_seq_size(base_ref, resolving_context);
        } else if !types::is_named(&base_ref) {
            // Not a named ref, no resolving needs to be done
            return base_ref;
        }

        let type_id = if let TypeRef::Named(id) = base_ref {
            id
        } else {
            unreachable!()
        };

        // Clone is used to appease the borrow checker so that `self` and
        // `self.type_table` aren't borrowed, allowing nested exprs
        // ???: Take type, automatically breaking chains?
        let mut type_info = self.type_table.get_type(type_id).clone();

        let replace_ref = match &mut type_info {
            Type::Alias { to } => self.resolve_type_alias(to, resolving_context),
            Type::Array {
                ranges,
                element_type,
                is_flexible,
                is_init_sized,
            } => self.resolve_type_array(
                ranges,
                element_type,
                *is_flexible,
                *is_init_sized,
                resolving_context,
            ),
            Type::Enum { .. } => None,      // Already resolved, do nothing
            Type::EnumField { .. } => None, // Already resolved, do nothing
            Type::Forward { is_resolved } => self.resolve_type_forward(is_resolved),
            Type::Function { params, result } => self.resolve_type_function(params, result),
            Type::Pointer { to, .. } => self.resolve_type_pointer(to),
            Type::Range {
                start,
                end,
                base_type,
            } => self.resolve_type_range(start, end, base_type, resolving_context),
            Type::Reference { expr } => self.resolve_type_reference(expr, resolving_context),
            Type::Set { range: index } => self.resolve_type_set(index),
            Type::SizeExpr { expr } => self.resolve_type_size(expr),
        };

        if replace_ref.is_none() {
            // Replace the type with the updated type
            self.type_table.replace_type(type_id, type_info);
        }

        return replace_ref.unwrap_or(base_ref);
    }

    fn resolve_type_alias(
        &mut self,
        to: &mut TypeRef,
        resolving_context: ResolveContext,
    ) -> ResolveResult {
        if types::is_error(to) {
            // Apply type error to `to`
            *to = TypeRef::TypeError;
        } else {
            // Resolve the `to`
            *to = self.resolve_type(*to, resolving_context);
        }

        // Nothing to replace
        None
    }

    fn resolve_type_size(&mut self, expr: &mut Box<Expr>) -> ResolveResult {
        // Visit the expr, producing a compile-time value
        let eval = self.visit_expr(expr);

        if !expr.is_compile_eval() || eval.is_none() {
            // Is not a compile-time expression, give back a type error
            self.reporter.report_error(
                expr.get_span(),
                format_args!("Expression is not a compile-time expression"),
            );
            return Some(TypeRef::TypeError);
        }

        // Value type-checking should be done by the destination type type

        // Type should be valid, replace with eval_value
        if let Some(value) = eval {
            let span = expr.get_span().clone();
            *expr = Box::new(
                Expr::try_from(value).expect("Cannot convert size value back into an expression"),
            );
            expr.set_span(span);
        }

        // Nothing to replace
        None
    }

    fn resolve_type_array(
        &mut self,
        ranges: &mut Vec<TypeRef>,
        element_type: &mut TypeRef,
        is_flexible: bool,
        is_init_sized: bool,
        resolving_context: ResolveContext,
    ) -> ResolveResult {
        // Resolve the ranges
        for range in ranges.iter_mut() {
            // Not required to be compile-time, unless we are in a compile-time context
            *range = self.resolve_type(*range, resolving_context);

            if matches!(resolving_context, ResolveContext::CompileTime(_))
                && !is_flexible
                && !is_init_sized
            {
                // If the following holds true
                // - The index type is a range,
                // - We are in a compile-time context,
                // - This is an explict sized array (i.e. not `flexible` nor `init` sized)
                // Check if it is a not a zero sized range
                if let Some(Type::Range { start, end, .. }) = self.type_table.type_from_ref(&range)
                {
                    // Not being `flexible` nor `init`-sized guarrantees that end is a `Some`
                    if !validate_range_size(&start, &end.as_ref().unwrap(), &self.type_table, false)
                    {
                        // Zero sized ranges aren't allowed in compile-time array types
                        let range_span = start.get_span().span_to(end.as_ref().unwrap().get_span());
                        self.reporter.report_error(
                            &range_span,
                            format_args!("Range bounds creates a zero-sized range"),
                        );
                        *range = TypeRef::TypeError;
                    }
                }
            }
        }

        // Resolve the element type
        // Required to be compile-time as the element size must be known
        *element_type = self.resolve_type(*element_type, ResolveContext::CompileTime(false));

        // Nothing to replace
        None
    }

    fn resolve_type_function(
        &mut self,
        params: &mut Option<Vec<ParamDef>>,
        result: &mut Option<TypeRef>,
    ) -> ResolveResult {
        // Resolve each of the parameters
        if params.is_some() {
            for param in params.as_mut().unwrap().iter_mut() {
                param.type_spec =
                    self.resolve_type(param.type_spec, ResolveContext::CompileTime(false));
            }
        }

        // Resolve the result type
        if result.is_some() {
            *result = Some(self.resolve_type(result.unwrap(), ResolveContext::CompileTime(false)));
        }

        // Nothing to replace
        None
    }

    fn resolve_type_range(
        &mut self,
        start: &mut Expr,
        end: &mut Option<Expr>,
        base_type: &mut TypeRef,
        resolving_context: ResolveContext,
    ) -> ResolveResult {
        // Base type starts out as a type error
        *base_type = TypeRef::TypeError;

        // Visit the bound expressions
        let left_eval = self.visit_expr(start);
        let right_eval = if end.is_some() {
            self.visit_expr(end.as_mut().unwrap())
        } else {
            None
        };

        // Apply the folded values
        if let Some(left) = left_eval {
            let span = start.get_span().clone();
            *start = Expr::try_from(left).expect("Cannot convert start bound into an expression");
            start.set_span(span);
        }

        if let Some(right) = right_eval {
            let span = start.get_span().clone();
            end.replace(
                Expr::try_from(right).expect("Cannot convert end bound into an expression"),
            );
            end.as_mut().unwrap().set_span(span);
        }

        if !start.is_compile_eval() {
            // The start range must be a compile-time expression
            // Span over the start bound
            self.reporter.report_error(
                start.get_span(),
                format_args!("Start bound must be a compile-time expression"),
            );

            // Produce a type error as this is not a valid expression
            return Some(TypeRef::TypeError);
        }

        if matches!(resolving_context, ResolveContext::CompileTime(_)) {
            // All type info must be known at compile-time

            // Validate that the range type ref references a range that
            // has the end bound as a compile-time expression
            // Don't need to worry about checking * (checked by the parser)
            if let Some(end) = end {
                if !end.is_compile_eval() {
                    // Right-hand side is not a compile-time expression
                    // Span over the end bound
                    self.reporter.report_error(
                        end.get_span(),
                        format_args!("End bound must be a compile-time expression"),
                    );

                    // Range is not a valid type
                    return Some(TypeRef::TypeError);
                }
            }
        }

        // Build a location spanning over the entire range
        let range_span = if end.is_some() {
            start.get_span().span_to(end.as_ref().unwrap().get_span())
        } else {
            start.get_span().clone()
        };

        // Try to derive a base copy from the given types
        let start_type = start.get_eval_type();
        let end_type = if end.is_some() {
            end.as_ref().unwrap().get_eval_type()
        } else {
            // No specified end range, use the start type
            start_type
        };

        if !types::is_equivalent_to(&start_type, &end_type, &self.type_table) {
            // Range eval types do not match
            self.reporter.report_error(&range_span, format_args!("Range bounds must be both integers, characters, booleans, or elements from the same enumeration"));

            return Some(TypeRef::TypeError);
        } else if (types::is_char_seq_type(&start_type)
            && types::get_sized_len(&start_type).unwrap_or(0) != 1)
            || (types::is_char_seq_type(&end_type)
                && types::get_sized_len(&end_type).unwrap_or(0) != 1)
        {
            // Range eval types are the wrong types
            self.reporter.report_error(&range_span, format_args!("Range bounds must be both integers, characters, booleans, or elements from the same enumeration"));

            return Some(TypeRef::TypeError);
        }

        // Check if the start and end bounds form a positive range size
        if end.is_some() && start.is_compile_eval() && end.as_ref().unwrap().is_compile_eval() {
            let is_valid_size =
                validate_range_size(&start, &end.as_ref().unwrap(), &self.type_table, true);

            if !is_valid_size {
                self.reporter.report_error(
                    &range_span,
                    format_args!("Range bounds creates a negative-sized range"),
                );
                return Some(TypeRef::TypeError);
            }
        }

        // Update the base type
        // Either `start_type` or `end_type` could be used
        *base_type = self.dealias_resolve_type(start_type);

        // If the base type is an enum field, take the associated enum type
        if let Some(Type::EnumField { enum_type, .. }) = self.type_table.type_from_ref(base_type) {
            *base_type = *enum_type;
        }

        // Nothing to replace
        None
    }

    fn resolve_type_reference(
        &mut self,
        expr: &mut Box<Expr>,
        resolving_context: ResolveContext,
    ) -> ResolveResult {
        // Reference will produce a reference to the associated type_spec
        // If there is no reference to a type, a TypeError is produced

        // Evaluate expression
        self.visit_expr(expr);

        // Error reporting purposes
        let reference_locate;

        // Ensure that the top-most expression resolves to a type
        match &**expr {
            Expr::Dot { field, .. } => {
                if !field.is_typedef {
                    self.reporter.report_error(
                        &field.token.location,
                        format_args!("Field '{}' is not a reference to a type", field.name),
                    );

                    // Produce a type error
                    return Some(TypeRef::TypeError);
                }

                reference_locate = field.token.location.clone();
            }
            Expr::Reference { ident } => {
                if !ident.is_typedef {
                    self.reporter.report_error(
                        &ident.token.location,
                        format_args!("'{}' is not a reference to a type", ident.name),
                    );

                    // Produce a type error
                    return Some(TypeRef::TypeError);
                }

                reference_locate = ident.token.location.clone();
            }
            _ => unreachable!(), // No other expressions allowed, handled by the parser
        }

        // Check if the eval type de-aliases to a forward
        let type_ref = self.dealias_resolve_type(expr.get_eval_type());

        if let Some(Type::Forward { is_resolved }) = self.type_table.type_from_ref(&type_ref) {
            if !*is_resolved {
                // The type is not resolved at all, replace with TypeError
                self.reporter.report_error(
                    &reference_locate,
                    format_args!("Type reference is not resolved in the current unit"),
                );
                return Some(TypeRef::TypeError);
            } else if matches!(resolving_context, ResolveContext::CompileTime(false)) {
                // The type ref is required to be resolved at this point, replace with TypeError
                self.reporter.report_error(
                    &reference_locate,
                    format_args!("Type reference is required to be resolved at this point"),
                );
                return Some(TypeRef::TypeError);
            }
        }

        // Produce the resolved type
        return Some(type_ref);
    }

    fn resolve_type_set(&mut self, index: &mut TypeRef) -> ResolveResult {
        // Keep track of the old type ref for error reporting
        let old_index_ref = *index;

        // Doesn't matter if the range is a type error or not, will be
        // ignored during equivalence checking
        *index = self.resolve_type(*index, ResolveContext::CompileTime(false));

        if types::is_named(&index) {
            // Check that the index reference is actually an index type and not a reference to a non-index type
            // Other cases are handled by the parser
            let real_index = self.dealias_resolve_type(*index);

            if !types::is_index_type(&real_index, &self.type_table) {
                // Not a real index type, change it to point to a type error
                *index = TypeRef::TypeError;

                // Report the error based on the reference location
                if let Some(Type::Reference { expr }) =
                    self.type_table.type_from_ref(&old_index_ref)
                {
                    self.reporter.report_error(
                        expr.get_span(),
                        format_args!("Set index is not a range, char, boolean, or enumerated type"),
                    );
                } else {
                    // Other cases should be reported by the parser (e.g. wrong primitive type)
                }
            }

            // If the index type is a range, check if it is a not a zero sized range
            if let Some(Type::Range { start, end, .. }) = self.type_table.type_from_ref(&real_index)
            {
                // Compile-time enforcement guarrantees that end is a some
                if !validate_range_size(&start, &end.as_ref().unwrap(), &self.type_table, false) {
                    // Zero sized ranges aren't allowed in set types
                    let range_span = start.get_span().span_to(end.as_ref().unwrap().get_span());
                    self.reporter.report_error(
                        &range_span,
                        format_args!("Range bounds creates a zero-sized range"),
                    );
                    *index = TypeRef::TypeError;
                }
            }
        } else if types::is_primitive(&index) && types::is_index_type(&index, &self.type_table) {
            // Is a primitive, either a 'char' or 'boolean'
            // Keep as is
        } else {
            // Ensure that the range is really a type error
            // Don't need to report, as it is covered by a previous error
            *index = TypeRef::TypeError;
        }

        // Nothing to replace
        None
    }

    fn resolve_type_forward(&mut self, is_resolved: &mut bool) -> ResolveResult {
        if !*is_resolved {
            // Type has not been resolved in the unit
            // Replace with a type error
            Some(TypeRef::TypeError)
        } else {
            // Type has been resolved in the unit, but will be replaced with the real type later
            None
        }
    }

    fn resolve_type_pointer(&mut self, to: &mut TypeRef) -> ResolveResult {
        // Resolve the 'to' type (allow forward references)
        *to = self.resolve_type(*to, ResolveContext::CompileTime(true));

        // Nothing to replace
        None
    }
}

/// Validates the range size
///
/// Specifiying `allow_zero_size` as true allows zero-sized ranges to be
/// captured as valid. Otherwise, zero-sized ranges are marked as invalid.
pub(super) fn validate_range_size(
    start_bound: &Expr,
    end_bound: &Expr,
    type_table: &TypeTable,
    allow_zero_size: bool,
) -> bool {
    let start_value =
        value::apply_ord(start_bound, type_table).expect("Cannot convert start bound into a value");
    let end_value =
        value::apply_ord(end_bound, type_table).expect("Cannot convert end bound into a value");

    if value::is_nat(&start_value) && value::is_nat(&end_value) {
        // Unsigned check (covers nat, char, boolean, and enum fields)
        let start_value: u64 = start_value.into();
        let end_value: u64 = end_value.into();

        let (inclusive_end_value, overflowed) = end_value.overflowing_add(1);

        // En - (St+1) = rs, rs < 0 is invalid, rs == 0 only valid if allowed

        // Check for overflow
        if overflowed {
            // end_value is u64::MAX
            // Nothing can be greater than u64::MAX, so this always forms a valid range
            return true;
        } else {
            let (range_size, range_overflow) = inclusive_end_value.overflowing_sub(start_value);

            // Overflow indicates a start bound greater than the adjusted end range
            return !range_overflow && (allow_zero_size || range_size != 0);
        }
    } else {
        // Check for int
        let start_value = value::value_as_i64(start_value);
        let end_value = value::value_as_i64(end_value);

        if start_value.is_err() && matches!(start_value, Err(ValueApplyError::Overflow)) {
            // Start value is definitely larger than the end bound, invalid
            false
        } else if end_value.is_err() && matches!(end_value, Err(ValueApplyError::Overflow)) {
            // End value is definitely larger than the end bound, valid
            true
        } else if !start_value.is_err() && !end_value.is_err() {
            // Values are in range, maybe
            let start_value = start_value.unwrap();
            let end_value = end_value.unwrap();
            let (inclusive_end_value, overflowed) = end_value.overflowing_add(1);

            // En - (St+1) = rs, rs < 0 is invalid, rs == 0 only valid if allowed

            // Check for overflow
            if overflowed {
                // end_value is i64::MAX
                // Any start bound less than `i64::MAX - 1` produces a negative sized range
                // Any start bound less than `i64::MAX` produces a zero sized range
                return start_value == i64::MAX || (start_value > i64::MAX - 1 && allow_zero_size);
            } else {
                let (range_size, range_overflow) = inclusive_end_value.overflowing_sub(start_value);

                // Overflow indicates a start bound greater than the adjusted end range
                // We check if the the range is not negative, as the zero-size
                // case is handled by the 3rd conditional
                return !range_overflow
                    && !(range_size < 0)
                    && (allow_zero_size || range_size != 0);
            }
        } else {
            // Wrong types, should be captured by type checks before this call
            return false;
        }
    }
}
