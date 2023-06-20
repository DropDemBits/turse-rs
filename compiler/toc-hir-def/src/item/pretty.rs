//! Pretty printing an item tree

use std::fmt;

use toc_source_graph::Package;

use crate::{
    item::{root_module, AnyItem, HasItems},
    Db, Mutability,
};

pub struct PrettyTree {
    root: PrettyItem,
}

pub fn render_item_tree(db: &dyn Db, root: Package) -> PrettyTree {
    fn render_sub_tree(db: &dyn Db, item: AnyItem) -> PrettyItem {
        match item {
            AnyItem::ConstVar(item) => PrettyItem {
                name: item.name(db).text(db),
                id: item.0.as_u32(),
                kind: PrettyItemKind::ConstVar(item.mutability(db)),
            },
            AnyItem::Module(item) => PrettyItem {
                name: item.name(db).text(db),
                id: item.0.as_u32(),
                kind: PrettyItemKind::Module(
                    item.items(db)
                        .iter()
                        .map(|child| render_sub_tree(db, (*child).into()))
                        .collect(),
                ),
            },
            AnyItem::RootModule(item) => PrettyItem {
                name: item.name(db).text(db),
                id: item.0.as_u32(),
                kind: PrettyItemKind::RootModule(
                    item.items(db)
                        .iter()
                        .map(|child| render_sub_tree(db, (*child).into()))
                        .collect(),
                ),
            },
            AnyItem::UnitModule(_) => unimplemented!(),
        }
    }

    let root = root_module(db, root);
    PrettyTree {
        root: render_sub_tree(db, AnyItem::RootModule(root)),
    }
}

struct PrettyItem {
    name: String,
    id: u32,
    kind: PrettyItemKind,
}

enum PrettyItemKind {
    ConstVar(Mutability),
    RootModule(Vec<PrettyItem>),
    Module(Vec<PrettyItem>),
}

impl PrettyTree {
    pub fn ensure_sorted(mut self) -> Self {
        fn sort_child(child: &mut PrettyItem) {
            match &mut child.kind {
                PrettyItemKind::ConstVar(_) => {}
                PrettyItemKind::RootModule(children) => {
                    children.sort_by_key(|item| item.id);
                    children.iter_mut().for_each(sort_child);
                }
                PrettyItemKind::Module(children) => {
                    children.sort_by_key(|item| item.id);
                    children.iter_mut().for_each(sort_child);
                }
            }
        }

        sort_child(&mut self.root);

        self
    }

    pub fn render_as_tree(self) -> String {
        fn render_item(out: &mut dyn fmt::Write, level: usize, item: PrettyItem) -> fmt::Result {
            let indent = "  ".repeat(level);
            let PrettyItem { name, id, kind } = item;

            match kind {
                PrettyItemKind::ConstVar(mutability) => {
                    let kind = match mutability {
                        Mutability::Const => "const",
                        Mutability::Var => "var",
                    };
                    writeln!(out, "{indent}{kind} {name} /* ConstVar({id}) */")?;
                    writeln!(out, "")?;
                }
                PrettyItemKind::RootModule(children) => {
                    writeln!(out, "{indent}package {name} /* RootModule({id}) */")?;
                    writeln!(out, "")?;
                    for child in children {
                        render_item(out, level + 1, child)?;
                    }
                }
                PrettyItemKind::Module(children) => {
                    writeln!(out, "{indent}module {name} /* Module({id}) */")?;
                    writeln!(out, "")?;
                    for child in children {
                        render_item(out, level + 1, child)?;
                    }
                }
            }

            Ok(())
        }

        let mut out = String::new();
        render_item(&mut out, 0, self.root).expect("failed to format tree");
        out
    }
}
