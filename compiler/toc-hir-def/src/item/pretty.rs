//! Pretty printing an item tree

use std::fmt;

use toc_source_graph::Package;

use crate::{
    item::{root_module, Item},
    Db, Mutability,
};

pub struct PrettyTree {
    root: PrettyItem,
}

pub fn render_item_tree(db: &dyn Db, root: Package) -> PrettyTree {
    fn render_sub_tree(db: &dyn Db, item: Item) -> PrettyItem {
        match item {
            Item::ConstVar(item) => {
                PrettyItem::ConstVar(item.mutability(db), item.name(db).text(db), item.0.as_u32())
            }
            Item::Module(item) => PrettyItem::Module(
                item.name(db).text(db),
                item.items(db)
                    .iter()
                    .map(|child| render_sub_tree(db, *child))
                    .collect(),
                item.0.as_u32(),
            ),
        }
    }

    let root = root_module(db, root);
    PrettyTree {
        root: render_sub_tree(db, Item::Module(root)),
    }
}

enum PrettyItem {
    ConstVar(Mutability, String, u32),
    Module(String, Vec<PrettyItem>, u32),
}

impl PrettyTree {
    pub fn render_as_tree(self) -> String {
        fn render_item(out: &mut dyn fmt::Write, level: usize, item: PrettyItem) -> fmt::Result {
            let indent = "  ".repeat(level);

            match item {
                PrettyItem::ConstVar(mutability, name, id) => {
                    let kind = match mutability {
                        Mutability::Const => "const",
                        Mutability::Var => "var",
                    };
                    writeln!(out, "{indent}{kind} {name} /* ConstVar({id}) */")?;
                    writeln!(out, "")?;
                }
                PrettyItem::Module(name, children, id) => {
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
