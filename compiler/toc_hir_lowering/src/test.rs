//! Tests for lowering

use toc_hir::stmt;

#[test]
fn lower_bare_var_def() {
    let parse = toc_parser::parse("var a := b");
    let lowered = crate::lower_ast(parse.syntax());

    let decl = &lowered.database.stmt_nodes.arena[lowered.unit.stmts[0]];
    assert!(matches!(
        decl,
        stmt::Stmt::ConstVar {
            is_pervasive: false,
            is_register: false,
            is_const: false,
            type_spec: None,
            ..
        }
    ))
}

#[test]
fn lower_simple_assignment() {
    let parse = toc_parser::parse("a := b");
    let lowered = crate::lower_ast(parse.syntax());

    let decl = &lowered.database.stmt_nodes.arena[lowered.unit.stmts[0]];
    assert!(matches!(
        decl,
        stmt::Stmt::Assign {
            op: stmt::AssignOp::None,
            ..
        }
    ))
}

#[test]
fn lower_compound_add_assignment() {
    let parse = toc_parser::parse("a += b");
    let lowered = crate::lower_ast(parse.syntax());

    let decl = &lowered.database.stmt_nodes.arena[lowered.unit.stmts[0]];
    assert!(
        matches!(
            decl,
            stmt::Stmt::Assign {
                op: stmt::AssignOp::Add,
                ..
            }
        ),
        "was {:?}",
        decl
    )
}
