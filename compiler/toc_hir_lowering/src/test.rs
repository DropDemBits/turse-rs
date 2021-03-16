//! Tests for lowering
use if_chain::if_chain;
use toc_hir::{expr, stmt};

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
    ));

    // Defs should be unique
    let (a_def, b_def) = if_chain! {
        if let stmt::Stmt::Assign { lhs, rhs, .. } = &lowered.database[lowered.unit.stmts[0]];
        if let (expr::Expr::Name(expr::Name::Name(a_id)), expr::Expr::Name(expr::Name::Name(b_id))) = (&lowered.database[*lhs], &lowered.database[*rhs]);
        then {
            (a_id.as_def(), b_id.as_def())
        } else {
            unreachable!();
        }
    };

    assert_ne!(a_def, b_def);
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

#[test]
fn lower_scoping_inner_use_outer_use() {
    let parse = toc_parser::parse("begin a := b end a := b");
    let lowered = crate::lower_ast(parse.syntax());

    // Grab use_id from inner scope
    let inner_use = if_chain! {
        if let stmt::Stmt::Block { stmts } = &lowered.database[lowered.unit.stmts[0]];
        if let stmt::Stmt::Assign { lhs, .. } = &lowered.database[stmts[0]];
        if let expr::Expr::Name(expr::Name::Name(use_id)) = &lowered.database[*lhs];
        then {
            *use_id
        } else {
            unreachable!();
        }
    };

    // Grab use_id from outer scope
    let outer_use = if_chain! {
        if let stmt::Stmt::Assign { lhs, .. } = &lowered.database[lowered.unit.stmts[1]];
        if let expr::Expr::Name(expr::Name::Name(use_id)) = &lowered.database[*lhs];
        then {
            *use_id
        } else {
            unreachable!();
        }
    };

    // Should be the same due to import boundary hoisting
    assert_eq!(outer_use.as_def(), inner_use.as_def());
}

#[test]
fn lower_scoping_outer_use_inner_use() {
    let parse = toc_parser::parse("q := j begin q := k end");
    let lowered = crate::lower_ast(parse.syntax());

    // Grab use_id from inner scope
    let inner_use = if_chain! {
        if let stmt::Stmt::Block { stmts } = &lowered.database[lowered.unit.stmts[1]];
        if let stmt::Stmt::Assign { lhs, .. } = &lowered.database[stmts[0]];
        if let expr::Expr::Name(expr::Name::Name(use_id)) = &lowered.database[*lhs];
        then {
            *use_id
        } else {
            unreachable!();
        }
    };

    // Grab use_id from outer scope
    let outer_use = if_chain! {
        if let stmt::Stmt::Assign { lhs, .. } = &lowered.database[lowered.unit.stmts[0]];
        if let expr::Expr::Name(expr::Name::Name(use_id)) = &lowered.database[*lhs];
        then {
            *use_id
        } else {
            unreachable!();
        }
    };

    assert_eq!(inner_use.as_def(), outer_use.as_def());
}
