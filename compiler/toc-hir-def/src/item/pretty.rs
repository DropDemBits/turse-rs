//! Pretty printing an item tree

use std::fmt;

use toc_source_graph::Package;

use crate::{
    Db, DisplayWithDb, Mutability,
    item::{AnyItem, ConstVar, HasItems, Module, RootModule, UnitModule, root_module},
};

impl<'db> DisplayWithDb<'db, dyn crate::Db> for ConstVar<'db> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, db: &dyn crate::Db) -> fmt::Result {
        let name = self.name(db).text(db).to_owned();
        let id = self.0.index();
        let kind = match self.mutability(db) {
            Mutability::Const => "const",
            Mutability::Var => "var",
        };

        write!(f, "{kind} {name} /* ConstVar({id}) */")
    }
}

impl<'db> DisplayWithDb<'db, dyn crate::Db> for RootModule<'db> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, db: &dyn crate::Db) -> fmt::Result {
        let name = self.name(db).text(db).to_owned();
        let id = self.0.index();

        write!(f, "package {name} /* RootModule({id}) */")
    }
}

impl<'db> DisplayWithDb<'db, dyn crate::Db> for UnitModule<'db> {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>, _db: &dyn crate::Db) -> fmt::Result {
        unimplemented!()
    }
}

impl<'db> DisplayWithDb<'db, dyn crate::Db> for Module<'db> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, db: &dyn crate::Db) -> fmt::Result {
        let name = self.name(db).text(db).to_owned();
        let id = self.0.index();

        write!(f, "module {name} /* Module({id}) */")
    }
}

impl<'db> DisplayWithDb<'db, dyn crate::Db> for AnyItem<'db> {
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
    db: &'db dyn Db,
    root: PrettyItem<'db>,
}

pub fn render_item_tree<'db>(db: &'db dyn Db, root: Package) -> PrettyTree<'db> {
    fn render_sub_tree<'db>(db: &'db dyn Db, item: AnyItem<'db>) -> PrettyItem<'db> {
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

    let root = toc_vfs_db::source_of(db, root.root(db).raw_path(db));
    let root = root_module(db, root);
    PrettyTree {
        db,
        root: render_sub_tree(db, AnyItem::RootModule(root)),
    }
}

impl<'db> PrettyTree<'db> {
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
        item: &PrettyItem<'db>,
    ) -> fmt::Result {
        let indent = "  ".repeat(level);
        writeln!(out, "{indent}{}", item.item.display(self.db))?;

        for child in &item.children {
            self.render_item(out, level + 1, child)?;
        }

        Ok(())
    }
}

struct PrettyItem<'db> {
    item: AnyItem<'db>,
    children: Vec<PrettyItem<'db>>,
}
