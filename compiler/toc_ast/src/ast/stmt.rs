//! Stmt AST Nodes
use crate::ast::expr::{BinaryOp, Expr};
use crate::ast::ident::IdentRef;
use crate::ast::types::Type;
use crate::scope::ScopeBlock;
use toc_core::Location;

/// Kinds of blocks
#[derive(Debug, Copy, Clone)]
pub enum BlockKind {
    /// Main block, root for all blocks, can be an execution block
    Main,
    /// Unit block, cannot be an execution block
    Unit,
    /// Inner block (begin ... end) inside of another block, or general statement block
    InnerBlock,
    /// Function block
    Function,
    /// Procedure block
    Procedure,
    /// Module block
    Module,
    /// Class block
    Class,
    /// Monitor block
    Monitor,
    /// Monitor-Class block, allows both class & monitor statments
    MonitorClass,
}

/// A grouping of statements.
/// Used in multiple statements.
#[derive(Debug, Clone)]
pub struct Block {
    /// Associated scope block
    pub block: ScopeBlock,
    /// Statements as part of the block
    pub stmts: Vec<Stmt>,
}

/// Statement Node Variant
#[derive(Debug, Clone)]
pub enum StmtKind {
    /// No-op statement, does nothing (produced by semi-colons)
    Nop,
    /// Error statement, produced if a valid statement could not be parsed (e.g. in nesting errors or unknown statments)
    Error,
    // Decls
    /// Variable & Constant declaration
    VarDecl {
        /// The identifier(s) declared
        /// If `None`, this statement is a no-op, and only present to allow access to the `type_spec` & `expr`
        idents: Option<Vec<IdentRef>>,
        /// The type spec for all of the identifiers
        type_spec: Option<Box<Type>>,
        /// The (semi-optional) initialization value
        value: Option<Box<Expr>>,
        /// If the declaration is for a const declaration
        is_const: bool,
        /// If the declared identifier(s) should be bound to a register
        bind_to_register: bool,
    },
    /// `type` statement declaration.
    /// The type_spec of `ident` is the declared type
    TypeDecl {
        /// The identifier associated with this type declare.
        /// If `None`, this statement is a no-op, and only allows access to the `resolved_type`
        ident: Option<IdentRef>,
        /// Type associated with the ident
        new_type: Box<Type>,
        /// If the identifier actually declares a new identifier
        is_new_def: bool,
    },
    // Stmts
    /// Simple & Compound assignment expression
    Assign {
        /// The variable reference expression
        var_ref: Box<Expr>,
        /// The (optional) assignment operation
        op: Option<BinaryOp>,
        /// The value to assign
        value: Box<Expr>,
    },
    /// Procedure or function call
    ProcedureCall {
        /// The reference to the procedure or function variable
        proc_ref: Box<Expr>,
    },
    /// Block of statements
    Block { block: Block },
    /// If statement and associated else(if)
    If {
        /// The condition of the if statement
        condition: Box<Expr>,
        /// Link to true branch statement
        true_branch: Box<Stmt>,
        /// Link to false branch statement
        false_branch: Option<Box<Stmt>>,
    },
}

/// Common statement node
#[derive(Debug, Clone)]
pub struct Stmt {
    /// The kind of statement node
    pub kind: StmtKind,
    /// The span of the statement
    pub span: Location,
}

mod pretty_print {
    use super::{Stmt, StmtKind};
    use crate::pretty_print;
    use std::fmt::{self, Write};

    impl fmt::Display for StmtKind {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                StmtKind::Nop => f.write_str("<nop>")?,
                StmtKind::Error => f.write_str("<error>")?,
                StmtKind::VarDecl {
                    is_const,
                    bind_to_register,
                    idents,
                    type_spec,
                    value,
                } => {
                    f.write_str(if *is_const { "const " } else { "var " })?;
                    if *bind_to_register {
                        f.write_str("register ")?;
                    }

                    f.write_char('[')?;
                    if let Some(idents) = idents {
                        pretty_print::print_list(f, idents.iter())?;
                    }
                    f.write_char(']')?;

                    if let Some(type_spec) = type_spec {
                        f.write_fmt(format_args!(" : {}", type_spec))?;
                    }

                    if let Some(value) = value {
                        f.write_fmt(format_args!(" := {}", value))?;
                    }
                }
                StmtKind::TypeDecl {
                    ident, new_type, ..
                } => {
                    f.write_str("type [")?;
                    if let Some(ident) = ident {
                        ident.fmt(f)?;
                    }
                    f.write_str("] : ")?;
                    new_type.fmt(f)?;
                }
                StmtKind::Assign { var_ref, op, value } => {
                    if let Some(op) = op {
                        f.write_fmt(format_args!("{0} {1}= {2}", var_ref, op, value))?;
                    } else {
                        f.write_fmt(format_args!("{0} := {1}", var_ref, value))?;
                    }
                }
                StmtKind::ProcedureCall { proc_ref } => proc_ref.fmt(f)?,
                StmtKind::Block { block } => {
                    // use '{' and '}' for block delimiter as it's nicer
                    f.write_char('{')?;

                    if !block.stmts.is_empty() {
                        f.write_char('\n')?;
                    }

                    {
                        let mut indenter = pretty_print::IndentWriter::new(f);

                        for stmt in &block.stmts {
                            indenter.write_fmt(format_args!("{}\n", stmt))?;
                        }
                    }

                    f.write_char('}')?;
                }
                StmtKind::If {
                    condition,
                    true_branch,
                    false_branch,
                } => {
                    f.write_fmt(format_args!("if ({}) then {}", condition, true_branch))?;

                    if let Some(false_branch) = false_branch {
                        f.write_fmt(format_args!("\nelse {}", false_branch))?;
                    }
                }
            }

            Ok(())
        }
    }

    impl fmt::Display for Stmt {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            self.kind.fmt(f)
        }
    }
}
