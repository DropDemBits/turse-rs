use std::{collections::VecDeque, fmt::Write};

use toc_source_graph::Package;

use crate::{
    Db, DisplayWithDb,
    expr::{Expr, ExprId, Name},
    item::{AnyItem, HasItems},
    stmt::{Stmt, StmtId},
};

pub fn render_package_bodies(db: &dyn Db, package: Package) -> String {
    // Collect all of the items in the package
    let items = {
        let root = toc_vfs_db::source_of(db, package.root(db).raw_path(db).as_path());
        let mut items = vec![];

        let mut to_explore = VecDeque::new();
        to_explore.push_back(AnyItem::RootModule(crate::item::root_module(db, root)));

        loop {
            let Some(item) = to_explore.pop_front() else {
                break;
            };

            match item {
                AnyItem::ConstVar(_) => continue,
                AnyItem::RootModule(item) => to_explore.extend(
                    item.child_items(db)
                        .items(db)
                        .iter()
                        .map(|item| AnyItem::from(*item)),
                ),
                AnyItem::UnitModule(_) => unimplemented!(),
                AnyItem::Module(item) => to_explore.extend(
                    item.child_items(db)
                        .items(db)
                        .iter()
                        .map(|item| AnyItem::from(*item)),
                ),
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

pub fn render_item_body<'db>(db: &'db dyn Db, item: AnyItem<'db>) -> String {
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

    for stmt in body.top_level_stmts(db) {
        writeln!(&mut out, "{}", render_stmt(db, *stmt, 1)).unwrap();
    }

    out
}

fn render_stmt<'db>(db: &'db dyn Db, stmt: StmtId<'db>, indent: usize) -> String {
    let (body, stmt) = (stmt.body(), stmt.stmt());
    let mut out = String::new();

    let indent_text = "  ".repeat(indent);
    out.push_str(&indent_text);

    match body.contents(db).stmt(stmt) {
        Stmt::InitializeConstVar(_item, _expr) => {
            write!(&mut out, "/* constvar initializer */").unwrap();
        }
        Stmt::InitializeBindItem(_item, _expr) => {
            write!(&mut out, "/* bind initializer */").unwrap();
        }
        Stmt::Assign(stmt) => {
            let lhs = render_expr(db, stmt.lhs.in_body(body), indent + 1);
            let op_name = match stmt.op {
                None => "Assign".to_owned(),
                Some(op) => format!("{op:?}Assign"),
            };
            let rhs = render_expr(db, stmt.rhs.in_body(body), indent + 1);
            write!(&mut out, "{op_name}\n{lhs}\n{rhs}").unwrap();
        }
        Stmt::Put(stmt) => {
            write!(&mut out, "put ").unwrap();
            if let Some(stream_num) = stmt.stream_num {
                write!(
                    &mut out,
                    ": {}, ",
                    render_expr(db, stream_num.in_body(body), indent)
                )
                .unwrap();
            }

            for put_item in &stmt.items {
                write!(&mut out, "\n{indent_text}  ").unwrap();
                match put_item {
                    crate::stmt::Skippable::Skip => write!(&mut out, "skip, ").unwrap(),
                    crate::stmt::Skippable::Item(put_item) => {
                        write!(
                            &mut out,
                            "PutItem({}",
                            render_expr(db, put_item.expr.in_body(body), indent + 1)
                        )
                        .unwrap();

                        if let Some(expr) = put_item.opts.width() {
                            write!(&mut out, "\n{indent_text}").unwrap();
                            write!(
                                &mut out,
                                "width: {}",
                                render_expr(db, expr.in_body(body), indent)
                            )
                            .unwrap()
                        }
                        if let Some(expr) = put_item.opts.precision() {
                            write!(&mut out, "\n{indent_text}").unwrap();
                            write!(
                                &mut out,
                                "precision: {}",
                                render_expr(db, expr.in_body(body), indent)
                            )
                            .unwrap()
                        }
                        if let Some(expr) = put_item.opts.exponent_width() {
                            write!(&mut out, "\n{indent_text}").unwrap();
                            write!(
                                &mut out,
                                "exponent_width: {}",
                                render_expr(db, expr.in_body(body), indent)
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

fn render_expr(db: &dyn Db, expr: ExprId, _indent: usize) -> String {
    let (body, expr) = (expr.body(), expr.expr());
    let resolutions = body.resolved_names(db);
    let mut out = String::new();

    match body.contents(db).expr(expr) {
        Expr::Missing => write!(&mut out, "<missing>").unwrap(),
        Expr::Literal(literal) => write!(&mut out, "{literal:?}").unwrap(),
        Expr::Init(_) => todo!(),
        Expr::Binary(_) => todo!(),
        Expr::Unary(_) => todo!(),
        Expr::All => todo!(),
        Expr::Range(_) => todo!(),
        Expr::Name(Name::Name(query)) => {
            let (name, scope_set) = &body.contents(db).queries[*query];
            let name = name.text(db);

            write!(&mut out, "{name}@({scope_set:#?}) -> ").unwrap();
            match resolutions.binding_of(db, *query) {
                Some(binding) => write!(&mut out, "{binding:?}").unwrap(),
                None => write!(&mut out, "<unresolved>").unwrap(),
            };
        }
        Expr::Name(Name::Self_) => write!(&mut out, "self").unwrap(),
        Expr::Field(_) => todo!(),
        Expr::Deref(_) => todo!(),
        Expr::Call(_) => todo!(),
    };

    out
}
