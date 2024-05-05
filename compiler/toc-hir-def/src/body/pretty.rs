use std::{collections::VecDeque, fmt::Write};

use toc_source_graph::Package;

use crate::{
    expr::{Expr, ExprId, Name},
    item::{AnyItem, HasItems},
    stmt::{Stmt, StmtId},
    Db, DisplayWithDb,
};

pub fn render_package_bodies(db: &dyn Db, package: Package) -> String {
    // Collect all of the items in the package
    let items = {
        let mut items = vec![];

        let mut to_explore = VecDeque::new();
        to_explore.push_back(AnyItem::RootModule(crate::item::root_module(db, package)));

        loop {
            let Some(item) = to_explore.pop_front() else {
                break;
            };

            match item {
                AnyItem::ConstVar(_) => continue,
                AnyItem::RootModule(item) => {
                    to_explore.extend(item.items(db).iter().map(|item| AnyItem::from(*item)))
                }
                AnyItem::UnitModule(_) => unimplemented!(),
                AnyItem::Module(item) => {
                    to_explore.extend(item.items(db).iter().map(|item| AnyItem::from(*item)))
                }
            }

            items.push(item);
        }

        items
    };

    let mut out = String::new();

    for item in items {
        writeln!(&mut out, "{}", render_item_body(db, item)).unwrap()
    }

    out
}

pub fn render_item_body(db: &dyn Db, item: AnyItem) -> String {
    let mut out = String::new();

    let body = match item {
        AnyItem::ConstVar(item) => panic!("exploring {item:?} which doesn't have a body"),
        AnyItem::RootModule(item) => {
            writeln!(&mut out, "body of {}", item.display(db)).unwrap();
            item.body(db)
        }
        AnyItem::UnitModule(_) => unimplemented!(),
        AnyItem::Module(item) => {
            writeln!(&mut out, "body of {}", item.display(db)).unwrap();
            item.body(db)
        }
    };

    for stmt in &**body.top_level_stmts(db) {
        writeln!(&mut out, "{}", render_stmt(db, *stmt, 1)).unwrap();
    }

    out
}

fn render_stmt(db: &dyn Db, stmt: StmtId, indent: usize) -> String {
    let (body, stmt) = (stmt.body(), stmt.stmt());
    let mut out = String::new();

    let indent_text = "  ".repeat(indent);
    out.push_str(&indent_text);

    match body.contents(db).stmt(stmt) {
        Stmt::InitializeConstVar(item, expr) => {
            write!(&mut out, "/* constvar initializer */").unwrap();
        }
        Stmt::InitializeBindItem(item, expr) => {
            write!(&mut out, "/* bind initializer */").unwrap();
        }
        Stmt::Assign(stmt) => {
            let lhs = render_expr(db, stmt.lhs.in_body(body));
            let op_name = match stmt.op {
                None => "Assign".to_owned(),
                Some(op) => format!("{op:?}Assign"),
            };
            let rhs = render_expr(db, stmt.rhs.in_body(body));
            write!(&mut out, "{lhs} {op_name} {rhs}").unwrap();
        }
        Stmt::Put(stmt) => {
            write!(&mut out, "put ").unwrap();
            if let Some(stream_num) = stmt.stream_num {
                write!(
                    &mut out,
                    ": {}, ",
                    render_expr(db, stream_num.in_body(body))
                )
                .unwrap();
            }

            for put_item in &stmt.items {
                match put_item {
                    crate::stmt::Skippable::Skip => write!(&mut out, "skip, ").unwrap(),
                    crate::stmt::Skippable::Item(put_item) => {
                        write!(
                            &mut out,
                            "PutItem({}",
                            render_expr(db, put_item.expr.in_body(body))
                        )
                        .unwrap();

                        if let Some(expr) = put_item.opts.width() {
                            write!(&mut out, ", width: {}", render_expr(db, expr.in_body(body)))
                                .unwrap()
                        }
                        if let Some(expr) = put_item.opts.precision() {
                            write!(
                                &mut out,
                                ", precision: {}",
                                render_expr(db, expr.in_body(body))
                            )
                            .unwrap()
                        }
                        if let Some(expr) = put_item.opts.exponent_width() {
                            write!(
                                &mut out,
                                ", exponent_width: {}",
                                render_expr(db, expr.in_body(body))
                            )
                            .unwrap()
                        }

                        write!(&mut out, ")").unwrap()
                    }
                }
            }

            if stmt.append_newline {
                write!(&mut out, " AppendNewline").unwrap()
            }
        }
        Stmt::Get(_) => todo!(),
        Stmt::For(_) => todo!(),
        Stmt::Loop(_) => todo!(),
        Stmt::Exit(_) => todo!(),
        Stmt::If(_) => todo!(),
        Stmt::Case(_) => todo!(),
        Stmt::Block(stmt) => {
            writeln!(&mut out, "begin").unwrap();
            for stmt in &*stmt.stmts {
                writeln!(
                    &mut out,
                    "{}",
                    render_stmt(db, stmt.in_body(body), indent + 1)
                )
                .unwrap();
            }
            write!(&mut out, "{indent_text}end").unwrap();
        }
        Stmt::Call(_) => todo!(),
        Stmt::Return(_) => todo!(),
        Stmt::Result(_) => todo!(),
    }

    out
}

fn render_expr(db: &dyn Db, expr: ExprId) -> String {
    let (body, expr) = (expr.body(), expr.expr());

    match body.contents(db).expr(expr) {
        Expr::Missing => "<missing>".to_string(),
        Expr::Literal(literal) => format!("{literal:?}"),
        Expr::Init(_) => todo!(),
        Expr::Binary(_) => todo!(),
        Expr::Unary(_) => todo!(),
        Expr::All => todo!(),
        Expr::Range(_) => todo!(),
        Expr::Name(Name::Name(symbol)) => {
            symbol.text(db).to_string()
        }
        Expr::Name(Name::Self_) => "self".to_string(),
        Expr::Field(_) => todo!(),
        Expr::Deref(_) => todo!(),
        Expr::Call(_) => todo!(),
    }
}
