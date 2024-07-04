//! Lowering from AST declaration nodes to semantic item representations

use toc_hir_expand::{AstLocations, SemanticLoc};
use toc_syntax::ast;

use crate::{
    item::{
        item_loc_map::{BlockItemsMap, ItemLocMap},
        BlockItems, ConstVar, Item, ItemCollection, Module, NestedItem, NestedItemCollection,
    },
    Db, Symbol,
};

pub(crate) fn to_immediate_items(db: &dyn Db, collection: NestedItemCollection) -> ItemCollection {
    let immediate_items = collection
        .items(db)
        .iter()
        .flat_map(|item| match item {
            super::NestedItem::Item(item) => Some(*item),
            super::NestedItem::Nested(_) => None,
        })
        .collect::<Vec<_>>()
        .into_boxed_slice();

    ItemCollection::new(db, immediate_items, collection.loc_map(db).clone())
}

/// Collects the immediately nested items from a [`ast::StmtList`]
pub(crate) fn collect_nested_items(
    db: &dyn Db,
    stmt_list: SemanticLoc<ast::StmtList>,
) -> NestedItemCollection {
    let ast_locations = stmt_list.file(db.up()).ast_locations(db.up());
    let stmt_list = stmt_list.to_node(db.up());

    let mut loc_map = ItemLocMap::new();
    let mut block_items_map = BlockItemsMap::new();
    let items = stmt_list
        .stmts()
        .flat_map(|stmt| nested_item(db, stmt, ast_locations, &mut loc_map, &mut block_items_map))
        .flatten()
        .collect::<Vec<_>>();

    loc_map.shrink_to_fit();
    block_items_map.shrink_to_fit();

    NestedItemCollection::new(db, items.into(), loc_map, block_items_map)
}

/// Lowers a potential item, and returns either the new item, or `None`
/// if we don't support lowering it yet or it's not a well-formed item
/// (mainly because it doesn't have a name).
///
/// Note that this means that we'll drop errors when a module doesn't
/// have a name, but the more pressing fix anyway should be to focus
/// on the error that a module doesn't have a name.
pub(crate) fn nested_item(
    db: &dyn Db,
    stmt: ast::Stmt,
    ast_locations: &AstLocations,
    loc_map: &mut ItemLocMap,
    block_items_map: &mut BlockItemsMap,
) -> Option<Vec<NestedItem<Item>>> {
    Some(match stmt {
        ast::Stmt::ConstVarDecl(constvar) => {
            let names = constvar.constvar_names().unwrap();

            let items = names
                .names()
                .map(|decl_name| {
                    let loc = ast_locations.get(&decl_name);
                    let name = decl_name.name().unwrap().identifier_token().unwrap();
                    let name = Symbol::new(db, name.text().to_owned());
                    let item = ConstVar::new(db, name, loc);
                    loc_map.insert(loc, item);

                    NestedItem::Item(Item::ConstVar(item))
                })
                .collect::<Vec<_>>();

            items
        }
        // ast::Stmt::TypeDecl(_) => todo!(),
        // ast::Stmt::BindDecl(_) => todo!(),
        // ast::Stmt::ProcDecl(_) => todo!(),
        // ast::Stmt::FcnDecl(_) => todo!(),
        // ast::Stmt::ProcessDecl(_) => todo!(),
        // ast::Stmt::ExternalDecl(_) => todo!(),
        // ast::Stmt::ForwardDecl(_) => todo!(),
        // ast::Stmt::DeferredDecl(_) => todo!(),
        // ast::Stmt::BodyDecl(_) => todo!(),
        ast::Stmt::ModuleDecl(module) => {
            let loc = ast_locations.get(&module);
            let name = module.name()?.identifier_token().unwrap();
            let name = Symbol::new(db, name.text().to_owned());
            let item = Module::new(db, name, loc);
            loc_map.insert(loc, item);

            vec![NestedItem::Item(Item::Module(item))]
        }
        // ast::Stmt::ClassDecl(_) => todo!(),
        // ast::Stmt::MonitorDecl(_) => todo!(),
        // ast::Stmt::ImportStmt(_) => todo!(),
        // ast::Stmt::ExportStmt(_) => todo!(),
        // ast::Stmt::PreprocGlob(_) => todo!(),

        // Build all of the nested block items.
        // These never touch the loc map
        ast::Stmt::BlockStmt(block) => vec![NestedItem::Nested(to_block_items(
            db,
            block.stmt_list().unwrap(),
            ast_locations,
            block_items_map,
        ))],
        ast::Stmt::ForStmt(block) => vec![NestedItem::Nested(to_block_items(
            db,
            block.stmt_list().unwrap(),
            ast_locations,
            block_items_map,
        ))],

        ast::Stmt::LoopStmt(block) => vec![NestedItem::Nested(to_block_items(
            db,
            block.stmt_list().unwrap(),
            ast_locations,
            block_items_map,
        ))],

        ast::Stmt::IfStmt(stmt) => {
            let mut nested_items = vec![];

            let mut if_body = stmt.if_body();

            while let Some(body) = if_body {
                if let Some(true_branch) = body.true_branch() {
                    nested_items.push(NestedItem::Nested(to_block_items(
                        db,
                        true_branch,
                        ast_locations,
                        block_items_map,
                    )));
                }

                if_body = match body.false_branch() {
                    Some(ast::FalseBranch::ElseifStmt(stmt)) => stmt.if_body(),
                    Some(ast::FalseBranch::ElseStmt(stmt)) => {
                        nested_items.push(NestedItem::Nested(to_block_items(
                            db,
                            stmt.stmt_list().unwrap(),
                            ast_locations,
                            block_items_map,
                        )));

                        None
                    }
                    None => None,
                };
            }

            nested_items
        }
        ast::Stmt::CaseStmt(stmt) => {
            let mut nested_items = vec![];

            for arm in stmt.case_arm() {
                nested_items.push(NestedItem::Nested(to_block_items(
                    db,
                    arm.stmt_list().unwrap(),
                    ast_locations,
                    block_items_map,
                )));
            }

            nested_items
        }
        _ => return None,
    })
}

fn to_block_items(
    db: &dyn Db,
    stmt_list: ast::StmtList,
    ast_locations: &AstLocations,
    block_items_map: &mut BlockItemsMap,
) -> BlockItems {
    let loc = ast_locations.get(&stmt_list);
    let block_item = BlockItems::new(db, loc);
    block_items_map.insert(loc, block_item);

    block_item
}
