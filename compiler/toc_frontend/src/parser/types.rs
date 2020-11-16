//! Parser fragment, parsing all type specifications
use super::{ParseResult, Parser};
use toc_core::token::TokenType;
use toc_ast::ast::expr::{Expr, ExprKind, Literal};
use toc_ast::ast::types::{SeqSize, Type, TypeKind};
use toc_ast::types::{self, ParamInfo, PrimitiveType, TypeRef};

impl<'s> Parser<'s> {
    // --- Type Parsing --- //
    /// Tries to parse the given type, returning `Type` with a `TypeKind::Error` if the parsing couldn't be salvaged.
    /// If an error type is produced, the token that caused the error is not consumed.
    ///
    /// `parse_context`     The token type describing where the type is being parsed
    pub(super) fn parse_type(&mut self, parse_context: &TokenType) -> ParseResult<Type> {
        // Update nesting depth
        self.type_nesting = self.type_nesting.saturating_add(1);

        if self.type_nesting > super::MAX_NESTING_DEPTH {
            // Over nesting limit
            self.reporter.borrow_mut().report_error(
                &self.current().location,
                format_args!("Implementation limit - Type specification is nested too deeply"),
            );

            self.type_nesting = self
                .type_nesting
                .checked_sub(1)
                .expect("Mismatched nesting counts");

            // Nom the token!
            self.next_token();

            let err_type = Type {
                kind: TypeKind::Error,
                type_ref: None,
                span: self.previous().location,
            };

            return err_type;
        }

        let type_spec = match &self.current().token_type {
            // Basic primitive types
            TokenType::Addressint => self.type_primitive(PrimitiveType::AddressInt),
            TokenType::Boolean => self.type_primitive(PrimitiveType::Boolean),
            TokenType::Int => self.type_primitive(PrimitiveType::Int),
            TokenType::Int1 => self.type_primitive(PrimitiveType::Int1),
            TokenType::Int2 => self.type_primitive(PrimitiveType::Int2),
            TokenType::Int4 => self.type_primitive(PrimitiveType::Int4),
            TokenType::Nat => self.type_primitive(PrimitiveType::Nat),
            TokenType::Nat1 => self.type_primitive(PrimitiveType::Nat1),
            TokenType::Nat2 => self.type_primitive(PrimitiveType::Nat2),
            TokenType::Nat4 => self.type_primitive(PrimitiveType::Nat4),
            TokenType::Real => self.type_primitive(PrimitiveType::Real),
            TokenType::Real4 => self.type_primitive(PrimitiveType::Real4),
            TokenType::Real8 => self.type_primitive(PrimitiveType::Real8),
            TokenType::Char => self.type_char_seq(true, parse_context),
            TokenType::String_ => self.type_char_seq(false, parse_context),

            // Compound primitives
            TokenType::Flexible if self.peek().token_type == TokenType::Array => {
                self.type_array(parse_context)
            }
            TokenType::Array => self.type_array(parse_context),
            TokenType::Unchecked
                if matches!(
                    self.peek().token_type,
                    TokenType::Pointer | TokenType::Caret
                ) =>
            {
                // Unchecked pointer type
                self.type_pointer(parse_context)
            }
            TokenType::Pointer | TokenType::Caret => {
                // Checked pointer type
                self.type_pointer(parse_context)
            }
            TokenType::Set => self.type_set(parse_context),
            TokenType::Function | TokenType::Procedure => {
                // subprogram_header

                // Consume function & the identifier (don't care about those)
                // "function" / "procedure"
                let is_function = matches!(self.next_token().token_type, TokenType::Function);
                // Identifier
                let _ = self.expects(
                    TokenType::Identifier,
                    format_args!("Expected identifier in function type declaration"),
                );

                self.type_function(parse_context, is_function)
            }
            TokenType::Enum => self.type_enum(parse_context),
            TokenType::Record => {
                self.reporter.borrow_mut().report_error(
                    &self.current().location,
                    format_args!("Record types are not parsed yet"),
                );
                self.next_token();

                Type {
                    kind: TypeKind::Error,
                    type_ref: None,
                    span: self.previous().location,
                }
            }
            TokenType::Union => {
                self.reporter.borrow_mut().report_error(
                    &self.current().location,
                    format_args!("Union types are not parsed yet"),
                );
                self.next_token();

                Type {
                    kind: TypeKind::Error,
                    type_ref: None,
                    span: self.previous().location,
                }
            }
            _ => {
                // Try to parse either a reference, or a range type
                let ref_or_range = self.type_reference_or_range(parse_context);

                if let TypeKind::Error = &ref_or_range.kind {
                    // Report the error
                    self.reporter.borrow_mut().report_error(
                        &ref_or_range.span,
                        format_args!(
                            "Unexpected '{}', expected a type specifier",
                            self.get_token_lexeme(self.current())
                        ),
                    );
                }

                ref_or_range
            }
        };

        // Decrement nesting depth
        self.type_nesting = self
            .type_nesting
            .checked_sub(1)
            .expect("Mismatched nesting counts");
        type_spec
    }

    /// Tries to parse a primitive type from the previous token, returning
    /// `TypeRef::TypeError` if the parsing couldn't be salvaged.
    /// If a `Type::TypeError` is produced, no further tokens have been consumed.
    ///
    /// This is only used to parse a primitive type in an indirection
    /// expression.
    ///
    /// `parse_context`     The token type describing where the type is being parsed
    pub(super) fn parse_primitive_type(&mut self, parse_context: &TokenType) -> Type {
        fn make_primitive(s: &Parser<'_>, primitive: PrimitiveType) -> Type {
            Type {
                kind: TypeKind::Primitive(primitive),
                type_ref: None,
                span: s.previous().location,
            }
        }

        match self.previous().token_type {
            TokenType::Addressint => make_primitive(self, PrimitiveType::AddressInt),
            TokenType::Boolean => make_primitive(self, PrimitiveType::Boolean),
            TokenType::Int => make_primitive(self, PrimitiveType::Int),
            TokenType::Int1 => make_primitive(self, PrimitiveType::Int1),
            TokenType::Int2 => make_primitive(self, PrimitiveType::Int2),
            TokenType::Int4 => make_primitive(self, PrimitiveType::Int4),
            TokenType::Nat => make_primitive(self, PrimitiveType::Nat),
            TokenType::Nat1 => make_primitive(self, PrimitiveType::Nat1),
            TokenType::Nat2 => make_primitive(self, PrimitiveType::Nat2),
            TokenType::Nat4 => make_primitive(self, PrimitiveType::Nat4),
            TokenType::Real => make_primitive(self, PrimitiveType::Real),
            TokenType::Real4 => make_primitive(self, PrimitiveType::Real4),
            TokenType::Real8 => make_primitive(self, PrimitiveType::Real8),
            TokenType::Char => self.type_char_seq(true, parse_context),
            TokenType::String_ => self.type_char_seq(false, parse_context),
            _ => {
                self.reporter.borrow_mut().report_error(
                    &self.previous().location,
                    format_args!("'{}' is not a primitive type", self.previous().token_type),
                );

                Type {
                    kind: TypeKind::Error,
                    type_ref: None,
                    span: self.previous().location,
                }
            }
        }
    }

    /// Parse a basic primitive type
    fn type_primitive(&mut self, primitive: PrimitiveType) -> Type {
        // Consume type token
        self.next_token();

        Type {
            kind: TypeKind::Primitive(primitive),
            type_ref: None,
            span: self.previous().location,
        }
    }

    /// Gets the size specifier for a char(n) or string(n)
    fn get_size_specifier(&mut self, parse_context: &TokenType) -> Result<SeqSize, ()> {
        let maybe_seq_size = match self.current().token_type {
            TokenType::NatLiteral(size)
                if matches!(self.peek().token_type, TokenType::RightParen) =>
            {
                // Is only a literal, and not part of an expression
                if size == 0 {
                    // Empty length, never valid
                    self.reporter.borrow_mut().report_error(
                        &self.current().location,
                        format_args!("Invalid maximum string length of '0'"),
                    );
                    Err(())
                } else if size as usize >= types::MAX_STRING_SIZE {
                    // Greater than max length, never valid
                    self.reporter.borrow_mut().report_error(
                        &self.current().location,
                        format_args!(
                            "'{}' is larger than or equal to the maximum string length of '{}' (after including the end byte)",
                            size,
                            types::MAX_STRING_SIZE
                        ),
                    );
                    Err(())
                } else {
                    // Make a literal expr
                    self.next_token();

                    let expr = Expr {
                        kind: ExprKind::Literal {
                            value: Literal::Nat(size),
                        },
                        eval_type: TypeRef::Primitive(PrimitiveType::Nat),
                        is_compile_eval: true,
                        span: self.previous().location,
                    };

                    Ok(SeqSize::Sized(Box::new(expr)))
                }
            }
            TokenType::Star if matches!(self.peek().token_type, TokenType::RightParen) => {
                // Star is always by itself, nothing else
                // Consume parsed type
                self.next_token();

                if matches!(parse_context, TokenType::Function | TokenType::Procedure) {
                    Ok(SeqSize::Any)
                } else {
                    self.reporter.borrow_mut().report_error(
                        &self.previous().location,
                        format_args!(
                            "Length specifier of '*' is only valid in subprogram parameter types"
                        ),
                    );

                    Err(())
                }
            }
            _ => {
                // Try to parse as a compile-time expression
                // Resolved at validator time
                let size_expr = self.expr();

                // Put the parsed expression into a box
                match size_expr.kind {
                    ExprKind::Error => {
                        self.reporter.borrow_mut().report_error(
                            &self.current().location,
                            format_args!("Length specifier is not a '*' or a non-zero compile time expression"),
                        );
                        Err(())
                    }
                    _ => Ok(SeqSize::Sized(Box::new(size_expr))),
                }
            }
        };

        // Expect a right paren after the length specifier
        let _ = self.expects(
            TokenType::RightParen,
            format_args!("Expected ')' after length specifier"),
        );

        maybe_seq_size
    }

    /// Parse character sequence (string, char, char(n))
    fn type_char_seq(&mut self, is_char_type: bool, parse_context: &TokenType) -> Type {
        // Nom 'char' or 'string'
        if matches!(
            self.current().token_type,
            TokenType::Char | TokenType::String_
        ) {
            // Optional nom because this may be a part of an indirection expression,
            // where the leading token is already consumed
            self.next_token();
        }

        let span = self.previous().location;

        // If left paren, construct sized type
        let kind = if self.optional(&TokenType::LeftParen) {
            // Try to get the size
            let parsed_size = self.get_size_specifier(parse_context);

            match parsed_size {
                Ok(size) if is_char_type => TypeKind::CharN { size },
                Ok(size) => TypeKind::StringN { size },
                // Try to return as the base type, for preserving validator semantics and preserving compatibility with Turing proper
                Err(_) if is_char_type => TypeKind::Primitive(PrimitiveType::Char),
                Err(_) => TypeKind::Primitive(PrimitiveType::String_),
            }
        } else {
            // Produce base versions
            if is_char_type {
                // Make single char type
                TypeKind::Primitive(PrimitiveType::Char)
            } else {
                // Make varsized type
                TypeKind::Primitive(PrimitiveType::String_)
            }
        };

        let span = span.span_to(&self.previous().location);

        Type {
            kind,
            type_ref: None,
            span,
        }
    }

    /// Parse pointer to another type
    fn type_pointer(&mut self, parse_context: &TokenType) -> Type {
        let span = self.current().location;

        // Consume optional 'unchecked' attribute
        let is_unchecked = self.optional(&TokenType::Unchecked);

        // Consume "pointer" or '^'
        if let TokenType::Pointer = &self.next_token().token_type {
            // Consume the "to"
            let _ = self.expects(TokenType::To, format_args!("Expected 'to' after 'pointer'"));
        }

        // Get the pointer to type
        let pointer_to = self.parse_type(parse_context);

        let kind = TypeKind::Pointer {
            to: Box::new(pointer_to),
            is_unchecked,
        };

        let span = span.span_to(&self.previous().location);

        Type {
            kind,
            type_ref: None,
            span,
        }
    }

    /// Parse procedure & function parameter specification & result type
    fn type_function(&mut self, parse_context: &TokenType, has_result: bool) -> Type {
        let span = self.current().location;

        let params = if self.optional(&TokenType::LeftParen) {
            // Parameter Declaration
            let mut params = vec![];

            if self.current().token_type != TokenType::RightParen {
                let param_context = if has_result {
                    &TokenType::Function
                } else {
                    &TokenType::Procedure
                };

                loop {
                    // Parse a parameter type
                    match self.current().token_type {
                        TokenType::Function | TokenType::Procedure => {
                            // Parse in the context of a function/procedure, as those allow omission of '()'
                            // for function subprograms
                            params.push(self.type_subprogram_param(param_context))
                        }
                        _ => params.append(&mut self.type_var_param(param_context)),
                    }

                    if self.current().token_type != TokenType::Comma {
                        // No more things to parse, or invalid token
                        break;
                    }

                    // Consume ','
                    self.next_token();
                }
            }

            let _ = self.expects(
                TokenType::RightParen,
                format_args!("Expected ')' after parameter declaration"),
            );

            Some(params)
        } else if has_result
            && matches!(
                parse_context,
                TokenType::Const | TokenType::Var | TokenType::Type
            )
        {
            // In the context of a function type declaration, which requires the '()'
            if matches!(self.previous().token_type, TokenType::Identifier) {
                self.reporter.borrow_mut().report_warning(
                    &self.previous().location,
                    format_args!(
                        "Function type declarations must specifiy '()' after the identifier"
                    ),
                );
            } else {
                // Identifier is not really needed in these situations, though
                // we still do so for compatibility
                self.reporter.borrow_mut().report_warning(
                    &self.previous().location,
                    format_args!(
                        "Function type declarations must specifiy '()' after '{}'",
                        &self.previous().token_type
                    ),
                );
            }

            // TODO: Change to a `Some(vec![])` once function declarations are parsed
            // We can't test bare function references until function declarations are parsed, so
            // preserve the bareness
            None
        } else {
            // Parameterless declaration, in an allowed context
            None
        };

        let result = if has_result {
            let _ = self.expects(
                TokenType::Colon,
                format_args!("Expected ':' before the result type"),
            );

            Some(Box::new(self.parse_type(parse_context)))
        } else {
            // No result type
            None
        };

        let kind = TypeKind::Function { params, result };

        let span = span.span_to(&self.previous().location);

        Type {
            kind,
            type_ref: None,
            span,
        }
    }

    /// Parses a sequence of function variable-type parameters, producing one or more parameter definitions
    fn type_var_param(&mut self, parse_context: &TokenType) -> Vec<(Box<Type>, ParamInfo)> {
        // "var"? "register"? identifier ( ',' identifier )* ':' "cheat"? type_spec

        // Attributes apply to all idents
        let pass_by_ref = self.attrib_var();
        let bind_to_register = self.attrib_register();
        let mut idents = vec![];

        // Gather all identifiers
        loop {
            let ident = self
                .expects(
                    TokenType::Identifier,
                    format_args!("Expected identifier for parameter name"),
                )
                .map_or_else(|_| String::new(), |ident| ident.get_lexeme().to_string());

            idents.push(ident);

            if !self.optional(&TokenType::Comma) {
                break;
            }
        }

        let _ = self.expects(
            TokenType::Colon,
            format_args!("Expected ':' after parameter name"),
        );

        let force_type = self.optional(&TokenType::Cheat);
        let type_spec = Box::new(self.parse_type(parse_context));

        // Unfold the ident list into the individual parameter types
        idents
            .into_iter()
            .map(|name| {
                let info = ParamInfo {
                    name,
                    pass_by_ref,
                    bind_to_register,
                    force_type,
                };

                (type_spec.clone(), info)
            })
            .collect()
    }

    /// Parses a single function subprogram-type parameter, producing one parameter definition
    fn type_subprogram_param(&mut self, parse_context: &TokenType) -> (Box<Type>, ParamInfo) {
        // "function" | "procedure" identifier param_list

        // Consume "function" or "procedure"
        let has_result = matches!(self.next_token().token_type, TokenType::Function);

        let name = self
            .expects(
                TokenType::Identifier,
                format_args!("Expected identifier for parameter name"),
            )
            .map_or_else(|_| String::new(), |ident| ident.get_lexeme().to_string());

        let type_spec = Box::new(self.type_function(parse_context, has_result));

        let info = ParamInfo {
            name,
            pass_by_ref: false,
            bind_to_register: false,
            force_type: false,
        };

        (type_spec, info)
    }

    /// Try to parse either a reference, or a range.
    /// Returns a `Type` with the parsed type.
    /// If the reference expression is an `ExprKind::Error`, a `Type` with
    /// `TypeKind::Error` is produced.
    fn type_reference_or_range(&mut self, parse_context: &TokenType) -> Type {
        // Bail out on err (i.e. too deep in the expr nesting or not a
        // expression at all)
        let primary_expr = self.expr();

        if matches!(primary_expr.kind, ExprKind::Error) {
            // Not a valid type reference, make an error type
            return Type {
                kind: TypeKind::Error,
                type_ref: None,
                span: *primary_expr.get_span(),
            };
        }

        if self.current().token_type == TokenType::Range {
            // Pass off to the range parser
            self.type_range_rest(primary_expr, parse_context)
        } else {
            // Parse as a reference type (resolved at validator time)
            // Validate that the expression only contains dot expressions or a reference
            let mut current_expr = &primary_expr;

            loop {
                match &current_expr.kind {
                    ExprKind::Dot { left, .. } => current_expr = &left, // Move through the chain
                    ExprKind::Reference { .. } | ExprKind::Error => break, // Reached the end of the dot expression (if it's an error, no need to report it)
                    _ => {
                        // Not completely a dot expression
                        self.reporter.borrow_mut().report_error(
                            &current_expr.get_span(),
                            format_args!("Expression is not a valid type reference"),
                        );

                        // Regardless if an expression is not a proper reference,
                        // always make it into a valid type as further expressions
                        // may refer to undefined expressions
                        break;
                    }
                }
            }

            Type {
                span: primary_expr.span,
                type_ref: None,
                kind: TypeKind::Reference {
                    ref_expr: Box::new(primary_expr),
                },
            }
        }
    }

    // Parse the rest of a range type
    // Will always produce a range
    fn type_range_rest(&mut self, start_range: Expr, parse_context: &TokenType) -> Type {
        // Consume range dots
        let range_tok = self.next_token();

        // A None for the range is only acceptable in array decls
        // Everywhere else is invalid
        let is_inferred_valid = *parse_context == TokenType::Array;

        // Parse the end range
        let end_range = match self.current().token_type {
            // Inferred range
            TokenType::Star => {
                let star_tok = self.next_token();

                if !is_inferred_valid {
                    // Report, but don't bail out
                    self.reporter.borrow_mut().report_error(
                        &star_tok.location,
                        format_args!("'*' as a range end is only valid in an array range"),
                    );
                }

                SeqSize::Any
            }
            // Explicit range
            _ => SeqSize::Sized(Box::new(self.expr())),
        };

        if let SeqSize::Sized(expr) = &end_range {
            if let ExprKind::Error = expr.kind {
                // End range is not a valid range
                self.reporter.borrow_mut().report_error(
                    &range_tok.location,
                    if is_inferred_valid {
                        format_args!("Expected expression or '*' after '..'")
                    } else {
                        format_args!("Expected expression after '..'")
                    },
                );
            }
        }

        let span = start_range.get_span().span_to(&self.previous().location);

        let kind = TypeKind::Range {
            start: Box::new(start_range),
            end: end_range,
        };

        Type {
            kind,
            type_ref: None,
            span,
        }
    }

    /// Parse a single index specificier
    fn type_index(&mut self, parse_context: &TokenType) -> Type {
        // "boolean" | "char" | type_ident (handles type_enum) | type_range
        match self.current().token_type {
            TokenType::Boolean | TokenType::Char => self.parse_type(parse_context),
            _ => self.type_reference_or_range(parse_context),
        }
    }

    /// Parse an array type
    fn type_array(&mut self, parse_context: &TokenType) -> Type {
        // "flexible"? "array" range_spec (',' range_spec)* "of" type_spec

        let span = self.current().location;

        let is_flexible = self.optional(&TokenType::Flexible);
        let flexible_tok = self.previous().clone();

        // Guarranteed to always be called with array as the current or next token
        let array_tok = self
            .expects(TokenType::Array, format_args!("!!! oopsies !!!"))
            .expect("'type_array' called without checking if next or current token is 'array'");

        let mut is_init_sized = false;
        let mut ranges = vec![];

        loop {
            let range = self.type_index(&TokenType::Array);

            if matches!(range.kind, TypeKind::Error) {
                self.reporter.borrow_mut().report_error(
                    &self.current().location,
                    format_args!("Expected a range specifier after ','"),
                );
                break;
            }

            if ranges.is_empty() {
                // Pushing the first range, check for implicit range
                if let TypeKind::Range { end, .. } = &range.kind {
                    // An array is init-sized if the first range is an implicit range
                    is_init_sized = matches!(end, SeqSize::Any);

                    if is_flexible && is_init_sized {
                        // Flexible array cannot have an implicit range specifier
                        self.reporter.borrow_mut().report_error(
                            &self.previous().location,
                            format_args!("Flexible array cannot have an implicit range specifier"),
                        );
                    }
                }

                // Add the range
                ranges.push(range);
            } else if is_init_sized {
                // Part of an init sized array, drop the extra ranges
                self.reporter.borrow_mut().report_error(
                    &self.previous().location,
                    format_args!("Extra range specifier found in implicit size array"),
                );
            } else {
                // Pushing in an additional range, where an additional range is accepted
                if let TypeKind::Range { end, .. } = &range.kind {
                    // Report if the range is an implicit range
                    if matches!(end, SeqSize::Any) {
                        self.reporter.borrow_mut().report_error(
                            &self.previous().location,
                            format_args!("'*' is only valid for the first range specifier"),
                        );
                    }
                }

                // Add the range
                ranges.push(range);
            }
            if !self.optional(&TokenType::Comma) {
                break;
            }
        }

        if ranges.is_empty() {
            self.reporter.borrow_mut().report_error(
                &self.current().location,
                format_args!("Expected a range specifier after 'array'"),
            );
        }

        let _ = self.expects(
            TokenType::Of,
            format_args!("Expected 'of' after the last range specifier"),
        );

        let element_type = Box::new(self.parse_type(&TokenType::Array));

        // Check if the current array is allowed in the current parsing context
        if matches!(
            parse_context,
            TokenType::Union
                | TokenType::Record
                | TokenType::Array
                | TokenType::Function
                | TokenType::Procedure
        ) {
            let context_name = match parse_context {
                TokenType::Union => "unions",
                TokenType::Record => "records",
                TokenType::Array => "array element type specifiers",
                TokenType::Function => "function parameters",
                TokenType::Procedure => "procedure parameters",
                _ => unreachable!(),
            };

            if is_flexible {
                self.reporter.borrow_mut().report_error(
                    &flexible_tok.location,
                    format_args!("Flexible arrays are not allowed inside of {}", context_name),
                );
            } else if is_init_sized {
                self.reporter.borrow_mut().report_error(
                    &array_tok.location,
                    format_args!(
                        "Implicit size arrays are not allowed inside of {}",
                        context_name
                    ),
                );
            }
        }

        // Build the type
        let span = span.span_to(&self.previous().location);

        Type {
            kind: TypeKind::Array {
                ranges,
                element_type,
                is_flexible,
                is_init_sized,
            },
            type_ref: None,
            span,
        }
    }

    /// Parse a set type
    fn type_set(&mut self, parse_context: &TokenType) -> Type {
        // Parse the entire set declaration
        let set_tok = self.next_token();
        let span = set_tok.location;

        // nab 'of'
        let _ = self.expects(TokenType::Of, format_args!("Expected 'of' after 'set'"));

        // Parse the index type!
        let range = Box::new(self.type_index(&TokenType::Set));

        let span = span.span_to(&self.previous().location);

        if *parse_context != TokenType::Type {
            // Only allowed in "type" statements
            self.reporter.borrow_mut().report_error(
                &span,
                format_args!("Set types can only be declared inside of 'type' statements"),
            );

            // While set types are only allowed in the 'type' stmt context (i.e.
            // we are in a var/const/parameter/index type spec), it is safe
            // behaviour to build a type up from what has been parsed as the
            // index type spec may refer to undefined identifiers.
            //
            // Dropping this type definition would mean dropping these already
            // parsed scope definitions, which would result in an inconsitent
            // state during the validation stage, and will cause in a panic
            // or an assertion fail.
            //
            // Therefore, we must still build the set type, even if we are not
            // in a 'type' stmt context.
        }

        Type {
            kind: TypeKind::Set { range },
            type_ref: None,
            span,
        }
    }

    // Parse an enumeration type
    fn type_enum(&mut self, parse_context: &TokenType) -> Type {
        // Parse the entire enum declaration first
        let enum_tok = self.next_token();
        let span = enum_tok.location;

        // nab '('
        let _ = self.expects(
            TokenType::LeftParen,
            format_args!("Expected '(' after 'enum'"),
        );

        // Grab the enum fields
        let mut fields = vec![];
        loop {
            let oops_text = self.get_token_lexeme(self.previous()).to_string();
            let current_text = self.get_token_lexeme(self.current()).to_string();

            let ident = self
                .expects(
                    TokenType::Identifier,
                    format_args!(
                        "Expected identifier after '{}' ('{}' is not an identifier)",
                        oops_text, current_text
                    ),
                )
                .map_or_else(|_| String::new(), |_| current_text);

            fields.push(ident);

            // If there is no comma, there's no more identifiers to nab
            if !self.optional(&TokenType::Comma) {
                // End of the value declarations
                break;
            }
        }

        // nab ')'
        let _ = self.expects(
            TokenType::RightParen,
            format_args!("Expected ')' after enumeration field declarations"),
        );

        let span = span.span_to(&self.previous().location);

        if *parse_context != TokenType::Type {
            // Only allowed in "type" statements
            self.reporter.borrow_mut().report_error(
                &span,
                format_args!("Enumerated types can only be declared inside of 'type' statements"),
            );

            // As the enumerated type is not behind a Type::Alias, there is no way
            // of referencing the type and therefore it is impossible to pass this
            // enumerated type to anywhere else. This behaviour is still
            // acceptable as we distinguish between variable/constant references
            // and type references
            //
            // Thus, it is safe to build up a type from what has been parsed to
            // maintain consistent behaviour as with set types.
        }

        Type {
            kind: TypeKind::Enum { fields },
            type_ref: None,
            span,
        }
    }
}
