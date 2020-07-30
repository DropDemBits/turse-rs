//! Parser fragment, parsing all type specifications
use super::Parser;
use crate::compiler::ast::Expr;
use crate::compiler::frontend::token::TokenType;
use crate::compiler::types::{self, ParamDef, PrimitiveType, SequenceSize, Type, TypeRef};
use std::collections::HashMap;

impl<'s> Parser<'s> {
    // --- Type Parsing --- //
    /// Tries to parse the given type, returning TypeRef::TypeError if the parsing couldn't be salvaged.
    /// If a TypeRef::TypeError is produced, the token that caused the error is not consumed
    ///
    /// `parse_context`         The token type describing where the type is being parsed
    pub(super) fn parse_type(&mut self, parse_context: &TokenType) -> TypeRef {
        match &self.current().token_type {
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
            TokenType::String_ | TokenType::Char => self.type_char_seq(parse_context),

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
            TokenType::Record => unimplemented!(),
            TokenType::Union => unimplemented!(),
            _ => {
                // Try to parse either a reference, or a range type
                let ref_or_range = self.type_reference_or_range(parse_context);

                if ref_or_range.is_err() {
                    if !ref_or_range.unwrap_err() {
                        // No parsing has been done yet, report at the current location
                        self.reporter.report_error(
                            &self.current().location,
                            format_args!(
                                "Unexpected '{}', expected a type specifier",
                                self.get_token_lexeme(self.current())
                            ),
                        );
                    }

                    // Return a type error
                    TypeRef::TypeError
                } else {
                    ref_or_range.ok().unwrap()
                }
            }
        }
    }

    /// Parse a basic primitive type
    pub(super) fn type_primitive(&mut self, primitive: PrimitiveType) -> TypeRef {
        // Consume name
        self.next_token();

        TypeRef::Primitive(primitive)
    }

    /// Gets the size specifier for a char(n) or string(n)
    pub(super) fn get_size_specifier(
        &mut self,
        parse_context: &TokenType,
    ) -> Result<SequenceSize, ()> {
        match self.current().token_type {
            TokenType::NatLiteral(size)
                if matches!(self.peek().token_type, TokenType::RightParen) =>
            {
                // Is only a literal, and not part of an expression
                if size == 0 {
                    // Empty length, never valid
                    self.reporter.report_error(
                        &self.current().location,
                        format_args!("Invalid maximum string length of '0'"),
                    );
                    Err(())
                } else if size as usize >= types::MAX_STRING_SIZE {
                    // Greater than max length, never valid
                    self.reporter.report_error(
                        &self.current().location,
                        format_args!(
                            "'{}' is larger than or equal to the maximum string length of '{}' (after including the end byte)",
                            size,
                            types::MAX_STRING_SIZE
                        ),
                    );
                    Err(())
                } else {
                    // Consume parsed type
                    self.next_token();
                    Ok(SequenceSize::Size(size as usize))
                }
            }
            TokenType::Star if matches!(self.peek().token_type, TokenType::RightParen) => {
                // Star is always by itself, nothing else
                // Consume parsed type
                self.next_token();

                if matches!(parse_context, TokenType::Function | TokenType::Procedure) {
                    Ok(SequenceSize::Size(0 as usize))
                } else {
                    self.reporter.report_error(
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

                // Expect a right paren after the expression
                if !matches!(self.current().token_type, TokenType::RightParen) {
                    // Let the caller handle it
                    return Err(());
                }

                // Put the parsed expression into a type table reference
                match size_expr {
                    Ok(expr) => Ok(SequenceSize::CompileExpr(
                        types::get_type_id(&self.declare_type(Type::SizeExpr {
                            expr: Box::new(expr),
                        }))
                        .unwrap(),
                    )),
                    Err(_) => {
                        self.reporter.report_error(
                            &self.current().location,
                            format_args!(
                        "Length specifier is not a '*' or a non-zero compile time expression"
                    ),
                        );
                        Err(())
                    }
                }
            }
        }
    }

    /// Parse character sequence (string, char, char(n))
    pub(super) fn type_char_seq(&mut self, parse_context: &TokenType) -> TypeRef {
        // Nom "string" / "char"
        let is_char_type = matches!(self.next_token().token_type, TokenType::Char);

        // If left paren, construct sized type
        if self.current().token_type == TokenType::LeftParen {
            self.next_token();

            // Try to get the size
            let parsed_size = self.get_size_specifier(parse_context);

            // Missing ) is recoverable
            let _ = self.expects(
                TokenType::RightParen,
                format_args!("Expected ')' after length specifier"),
            );

            if is_char_type {
                match parsed_size {
                    Ok(size) => TypeRef::Primitive(PrimitiveType::CharN(size)),
                    // Try to return as a single char, for preserving validator semantics and preserving compatibility with Turing proper
                    Err(_) => TypeRef::Primitive(PrimitiveType::Char),
                }
            } else {
                match parsed_size {
                    Ok(size) => TypeRef::Primitive(PrimitiveType::StringN(size)),
                    // Try to return as a normal string, for preserving validator semantics
                    Err(_) => TypeRef::Primitive(PrimitiveType::String_),
                }
            }
        } else {
            // Produce bracketless versions
            if is_char_type {
                // Make single char type
                TypeRef::Primitive(PrimitiveType::Char)
            } else {
                // Make varsized type
                TypeRef::Primitive(PrimitiveType::String_)
            }
        }
    }

    /// Parse pointer to another type
    pub(super) fn type_pointer(&mut self, parse_context: &TokenType) -> TypeRef {
        // Consume optional 'unchecked' attribute
        let is_unchecked = self.optional(TokenType::Unchecked);

        // Consume "pointer" or '^'
        if let TokenType::Pointer = &self.next_token().token_type {
            // Consume the "to"
            let _ = self.expects(TokenType::To, format_args!("Expected 'to' after 'pointer'"));
        }

        // Get the pointer to type
        let pointer_to = self.parse_type(parse_context);
        let typedef = Type::Pointer {
            to: pointer_to,
            is_unchecked,
        };

        self.declare_type(typedef)
    }

    /// Parse procedure & function parameter specification & result type
    pub(super) fn type_function(&mut self, parse_context: &TokenType, has_result: bool) -> TypeRef {
        let param_decl = if let TokenType::LeftParen = self.current().token_type {
            // Parameter Declaration

            // Consume left paren
            self.next_token();

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
        } else {
            // Parameterless declaration
            None
        };

        if has_result
            && param_decl.is_none()
            && matches!(
                parse_context,
                TokenType::Const | TokenType::Var | TokenType::Type
            )
        {
            // In the context of a function type declaration, which requires the '()'
            if matches!(self.previous().token_type, TokenType::Identifier) {
                self.reporter.report_error(
                    &self.previous().location,
                    format_args!(
                        "Function type declarations must specifiy '()' after the identifier"
                    ),
                );
            } else {
                // Identifier is not really needed in these situations, though
                // we still do so for compatibility
                self.reporter.report_error(
                    &self.previous().location,
                    format_args!(
                        "Function type declarations must specifiy '()' after '{}'",
                        &self.previous().token_type
                    ),
                );
            }
        }

        let result_type = if has_result {
            let _ = self.expects(
                TokenType::Colon,
                format_args!("Expected ':' before the result type"),
            );

            Some(self.parse_type(parse_context))
        } else {
            // No result type
            None
        };

        self.declare_type(Type::Function {
            params: param_decl,
            result: result_type,
        })
    }

    /// Parses a sequence of function variable-type parameters, producing one or more parameter definitions
    pub(super) fn type_var_param(&mut self, parse_context: &TokenType) -> Vec<ParamDef> {
        // "var"? "register"? identifier ( ',' identifier )* ':' "cheat"? type_spec

        // Attributes apply to all idents
        let pass_by_ref = self.optional(TokenType::Var);
        let bind_to_register = self.optional(TokenType::Register);
        let mut idents = vec![];

        // Gather all identifiers
        loop {
            let ident = self
                .expects(
                    TokenType::Identifier,
                    format_args!("Expected identifier for parameter name"),
                )
                .map(|tok| tok.location.get_lexeme(self.source).to_string())
                .unwrap_or(String::from("<invalid>"));

            idents.push(ident);

            if !self.optional(TokenType::Comma) {
                break;
            }
        }

        let _ = self.expects(
            TokenType::Colon,
            format_args!("Expected ':' after parameter name"),
        );

        let force_type = self.optional(TokenType::Cheat);
        let type_spec = self.parse_type(parse_context);

        // Unfold the ident list into the individual parameter types
        idents
            .into_iter()
            .map(|name| ParamDef {
                name,
                type_spec,
                pass_by_ref,
                bind_to_register,
                force_type,
            })
            .collect()
    }

    /// Parses a single function subprogram-type parameter, producing one parameter definition
    pub(super) fn type_subprogram_param(&mut self, parse_context: &TokenType) -> ParamDef {
        // "function" | "procedure" identifier param_list

        // Consume "function" or "procedure"
        let has_result = matches!(self.next_token().token_type, TokenType::Function);

        let name = self
            .expects(
                TokenType::Identifier,
                format_args!("Expected identifier for parameter name"),
            )
            .map(|tok| tok.location.get_lexeme(self.source).to_string())
            .unwrap_or(String::from("<invalid>"));

        let type_spec = self.type_function(parse_context, has_result);

        ParamDef {
            name,
            type_spec,
            pass_by_ref: false,
            bind_to_register: false,
            force_type: false,
        }
    }

    /// Try to parse either a reference, or a range.
    /// Returns an `Ok(TypeRef)` with the parsed type, or an `Err(bool)`, with
    /// the `bool` indicating whether any type parsing has been attempted
    pub(super) fn type_reference_or_range(
        &mut self,
        parse_context: &TokenType,
    ) -> Result<TypeRef, bool> {
        // Bail out on err
        let primary_expr = self.expr().map_err(|_| false)?;

        if self.current().token_type == TokenType::Range {
            // Pass off to the range parser
            Ok(self.type_range_rest(primary_expr, parse_context))
        } else {
            // Parse as a reference type (resolved at validator time)
            // Validate that the expression only contains dot expressions or a reference
            let mut current_expr = &primary_expr;

            loop {
                match current_expr {
                    Expr::Dot { left, .. } => current_expr = &left, // Move through the chain
                    Expr::Reference { .. } => break, // Reached the end of the dot expression
                    _ => {
                        // Not completely a dot expression
                        // TODO: Location is incorrect, use expr span?
                        self.reporter.report_error(
                            &self.previous().location,
                            format_args!("Expression is not a valid type reference"),
                        );

                        // Indicate that the error has been reported
                        return Err(true);
                    }
                }
            }

            Ok(self.declare_type(Type::Reference {
                expr: Box::new(primary_expr),
            }))
        }
    }

    // Parse the rest of a range type
    // Will always produce a range
    pub(super) fn type_range_rest(
        &mut self,
        start_range: Expr,
        parse_context: &TokenType,
    ) -> TypeRef {
        // A None for the range is only acceptable in array decls
        // Everywhere else is invalid
        let is_inferred_valid = *parse_context == TokenType::Array;

        // Consume range dots
        let range_tok = self.next_token();

        // Parse the end range
        let end_range = match self.current().token_type {
            // Inferred range
            TokenType::Star => {
                let star_tok = self.next_token();

                if is_inferred_valid {
                    Ok(None)
                } else {
                    self.reporter.report_error(
                        &star_tok.location,
                        format_args!("'*' as a range end is only valid in an array range"),
                    );

                    // Bail out completely
                    return TypeRef::TypeError;
                }
            }
            // Explicit range
            _ => self.expr().map(|expr| Some(expr)),
        };

        match end_range {
            Err(_) => {
                self.reporter.report_error(
                    &range_tok.location,
                    if is_inferred_valid {
                        format_args!("Expected expression or '*' after '..'")
                    } else {
                        format_args!("Expected expression after '..'")
                    },
                );

                TypeRef::TypeError
            }
            Ok(end_range) => self.declare_type(Type::Range {
                start: start_range,
                end: end_range,
                base_type: TypeRef::Unknown,
            }),
        }
    }

    /// Parse a single index specificier
    pub(super) fn type_index(&mut self, parse_context: &TokenType) -> Result<TypeRef, ()> {
        // "boolean" | "char" | type_ident (handles type_enum) | type_range
        match self.current().token_type {
            TokenType::Boolean | TokenType::Char => Ok(self.parse_type(parse_context)),
            _ => self.type_reference_or_range(parse_context).map_err(|_| ()),
        }
    }

    /// Parse an array type
    pub(super) fn type_array(&mut self, parse_context: &TokenType) -> TypeRef {
        // "flexible"? "array" range_spec (',' range_spec)* "of" type_spec

        let is_flexible = self.optional(TokenType::Flexible);
        let flexible_tok = self.previous().clone();

        // Guarranteed to always be called with array as the current or next token
        let array_tok = self
            .expects(TokenType::Array, format_args!("!!! oopsies !!!"))
            .expect("'type_array' called without checking if next or current token is 'array'");

        let mut is_init_sized = false;
        let mut ranges = vec![];

        loop {
            let range = self.type_index(&TokenType::Array);

            if range.is_err() {
                let _ = self.reporter.report_error(
                    &self.current().location,
                    format_args!("Expected a range specifier after ','"),
                );
                break;
            }

            let range = range.ok().unwrap();

            if ranges.is_empty() {
                // Pushing the first range, check for implicit range
                if let Some(Type::Range { ref end, .. }) =
                    self.unit.as_ref().unwrap().types().type_from_ref(&range)
                {
                    if is_flexible && end.is_none() {
                        // Flexible array cannot have an implicit range specifier
                        self.reporter.report_error(
                            &self.previous().location,
                            format_args!("Flexible array cannot have an implicit range specifier"),
                        );
                    }

                    // An array is init-sized if the first range is an implicit range
                    is_init_sized = end.is_none();
                }
                // Add the range
                ranges.push(range);
            } else if is_init_sized {
                // Part of an init sized array, drop the extra ranges
                self.reporter.report_error(
                    &self.previous().location,
                    format_args!("Extra range specifier found in implicit size array"),
                );
            } else {
                // Pushing in an additional range, where an additional range is accepted
                if let Some(Type::Range { ref end, .. }) =
                    self.unit.as_ref().unwrap().types().type_from_ref(&range)
                {
                    // Report if the range is an implicit range
                    if end.is_none() {
                        self.reporter.report_error(
                            &self.previous().location,
                            format_args!("'*' is only valid for the first range specifier"),
                        );
                    }
                }

                // Add the range
                ranges.push(range);
            }

            if !self.optional(TokenType::Comma) {
                break;
            }
        }

        if ranges.is_empty() {
            self.reporter.report_error(
                &self.current().location,
                format_args!("Expected a range specifier after 'array'"),
            );
        }

        let _ = self.expects(
            TokenType::Of,
            format_args!("Expected 'of' after the last range specifier"),
        );

        let element_type = self.parse_type(&TokenType::Array);

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
                self.reporter.report_error(
                    &flexible_tok.location,
                    format_args!("Flexible arrays are not allowed inside of {}", context_name),
                );
            } else if is_init_sized {
                self.reporter.report_error(
                    &array_tok.location,
                    format_args!(
                        "Implicit size arrays are not allowed inside of {}",
                        context_name
                    ),
                );
            }
        }

        // Build the type
        self.declare_type(Type::Array {
            ranges,
            element_type,
            is_flexible,
            is_init_sized,
        })
    }

    /// Parse a set type
    pub(super) fn type_set(&mut self, parse_context: &TokenType) -> TypeRef {
        // Parse the entire set declaration
        let set_tok = self.next_token();
        // nab 'of'
        let _ = self.expects(TokenType::Of, format_args!("Expected 'of' after 'set'"));

        // Parse the index type!
        let range = match self.type_index(&TokenType::Set) {
            Err(_) => TypeRef::TypeError,
            Ok(range) => range,
        };

        if *parse_context != TokenType::Type {
            // Only allowed in "type" statements
            self.reporter.report_error(
                &set_tok.location,
                format_args!("Set types can only be declared inside of 'type' statements"),
            );
            TypeRef::TypeError
        } else {
            self.declare_type(Type::Set { range })
        }
    }

    // Parse an enumeration type
    pub(super) fn type_enum(&mut self, parse_context: &TokenType) -> TypeRef {
        // Parse the entire enum declaration first
        let enum_tok = self.next_token();
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
                .map(|_| current_text)
                .unwrap_or(String::from(""));

            fields.push(ident);

            // If there is no comma, there's no more identifiers to nab
            if !self.optional(TokenType::Comma) {
                // End of the value declarations
                break;
            }
        }

        // nab ')'
        let _ = self.expects(
            TokenType::RightParen,
            format_args!("Expected ')' after enumeration field declarations"),
        );

        if *parse_context != TokenType::Type {
            // Only allowed in "type" statements
            self.reporter.report_error(
                &enum_tok.location,
                format_args!("Enumerated types can only be declared inside of 'type' statements"),
            );

            return TypeRef::TypeError;
        }

        // Build the type from the identifiers
        // Create a dummy type
        let enum_type = self.declare_type(Type::Alias {
            to: TypeRef::Unknown,
        });

        // Create types for each of the fields and put them into a HashMap
        let mut enum_fields = HashMap::new();
        fields.into_iter().enumerate().for_each(|(ordinal, name)| {
            if !name.is_empty() {
                // Add all of the valid fields
                let field = self.declare_type(Type::EnumField { enum_type, ordinal });
                enum_fields.insert(name, field);
            }
        });

        // Replace the enum type with the real type
        self.replace_type(
            &enum_type,
            Type::Enum {
                fields: enum_fields,
            },
        );

        enum_type
    }
}
