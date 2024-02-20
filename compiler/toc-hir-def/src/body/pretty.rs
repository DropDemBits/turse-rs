use std::{collections::VecDeque, fmt::Write};

use toc_source_graph::Package;

use crate::{
    item::{AnyItem, HasItems},
    Db,
};

pub fn render_package_bodies(db: &dyn Db, package: Package) -> String {
    // Collect all of the items in the package
    let items = {
        let mut items = vec![];

        let mut to_explore = VecDeque::new();
        to_explore.push_back(AnyItem::RootModule(crate::item::root_module(db, package)));

        loop {
            let Some(item) = to_explore.pop_front() else { break };
            items.push(item);

            match item {
                AnyItem::ConstVar(_) => {}
                AnyItem::RootModule(item) => {
                    to_explore.extend(item.items(db).iter().map(|item| AnyItem::from(*item)))
                }
                AnyItem::UnitModule(_) => unimplemented!(),
                AnyItem::Module(item) => {
                    to_explore.extend(item.items(db).iter().map(|item| AnyItem::from(*item)))
                }
            }
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
    "".into()
}
