//! Pretty printing an item tree

use std::fmt;

use toc_source_graph::Package;

use crate::{
    Db, DisplayWithDb, Mutability,
    item::{AnyItem, ConstVar, HasItems, Module, RootModule, UnitModule, root_module},
};

impl<'db> DisplayWithDb<'db, dyn crate::Db + 'db> for ConstVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, db: &dyn crate::Db) -> fmt::Result {
        let name = self.name(db).text(db).to_owned();
        let id = self.0.as_u32();
        let kind = match self.mutability(db) {
            Mutability::Const => "const",
            Mutability::Var => "var",
        };

        write!(f, "{kind} {name} /* ConstVar({id}) */")
    }
}

impl<'db> DisplayWithDb<'db, dyn crate::Db + 'db> for RootModule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, db: &dyn crate::Db) -> fmt::Result {
        let name = self.name(db).text(db).to_owned();
        let id = self.0.as_u32();

        write!(f, "package {name} /* RootModule({id}) */")
    }
}

impl<'db> DisplayWithDb<'db, dyn crate::Db + 'db> for UnitModule {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>, _db: &dyn crate::Db) -> fmt::Result {
        unimplemented!()
    }
}

impl<'db> DisplayWithDb<'db, dyn crate::Db + 'db> for Module {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, db: &dyn crate::Db) -> fmt::Result {
        let name = self.name(db).text(db).to_owned();
        let id = self.0.as_u32();

        write!(f, "module {name} /* Module({id}) */")
    }
}

impl<'db> DisplayWithDb<'db, dyn crate::Db + 'db> for AnyItem {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, db: &dyn crate::Db) -> fmt::Result {
        match self {
            AnyItem::ConstVar(it) => it.fmt(f, db),
            AnyItem::RootModule(it) => it.fmt(f, db),
            AnyItem::UnitModule(it) => it.fmt(f, db),
            AnyItem::Module(it) => it.fmt(f, db),
        }
    }
}

pub struct PrettyTree<'db> {
    db: &'db (dyn Db + 'db),
    root: PrettyItem,
}

pub fn render_item_tree(db: &dyn Db, root: Package) -> PrettyTree {
    fn render_sub_tree(db: &dyn Db, item: AnyItem) -> PrettyItem {
        let children = match item {
            AnyItem::ConstVar(_) => {
                vec![]
            }
            AnyItem::Module(item) => item
                .items(db)
                .iter()
                .map(|child| render_sub_tree(db, (*child).into()))
                .collect(),
            AnyItem::RootModule(item) => item
                .items(db)
                .iter()
                .map(|child| render_sub_tree(db, (*child).into()))
                .collect(),
            AnyItem::UnitModule(_) => unimplemented!(),
        };

        PrettyItem { item, children }
    }

    let root = root_module(db, root);
    PrettyTree {
        db,
        root: render_sub_tree(db, AnyItem::RootModule(root)),
    }
}

impl PrettyTree<'_> {
    pub fn ensure_sorted(mut self) -> Self {
        fn sort_child(child: &mut PrettyItem) {
            child.children.sort_by_key(|item| item.item);
            child.children.iter_mut().for_each(sort_child);
        }

        sort_child(&mut self.root);

        self
    }

    pub fn render_as_tree(self) -> String {
        let mut out = String::new();
        self.render_item(&mut out, 0, &self.root)
            .expect("failed to format tree");
        out
    }

    fn render_item(
        &self,
        out: &mut dyn fmt::Write,
        level: usize,
        item: &PrettyItem,
    ) -> fmt::Result {
        let indent = "  ".repeat(level);
        writeln!(out, "{indent}{}", item.item.display(self.db))?;

        for child in &item.children {
            self.render_item(out, level + 1, child)?;
        }

        Ok(())
    }
}

struct PrettyItem {
    item: AnyItem,
    children: Vec<PrettyItem>,
}
