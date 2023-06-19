//! Lowering from AST declaration nodes to semantic item representations

use toc_hir_expand::{AstLocations, SemanticLoc};
use toc_syntax::ast;

use crate::{
    item::{item_loc_map::ItemLocMap, ItemsWithLocMap, Module, ModuleOrigin},
    Db, Symbol,
};

use super::{ConstVar, ConstVarOrigin, Item};

/// Collects the immediately accessible items from a [`ast::StmtList`]
pub(crate) fn collect_items(db: &dyn Db, stmt_list: SemanticLoc<ast::StmtList>) -> ItemsWithLocMap {
    let ast_locations = stmt_list.file(db.up()).ast_locations(db.up());
    let stmt_list = stmt_list.to_node(db.up());

    let mut loc_map = ItemLocMap::new();
    let items = stmt_list
        .stmts()
        .filter_map(|stmt| item(db, stmt, ast_locations, &mut loc_map))
        .flatten()
        .collect::<Vec<_>>();

    loc_map.shrink_to_fit();

    ItemsWithLocMap::new(db, items.into(), loc_map)
}

/// Lowers a potential item, and returns either the new item, or `None`
/// if we don't support lowering it yet or it's not a well-formed item
/// (mainly because it doesn't have a name).
///
/// Note that this means that we'll drop errors when a module doesn't
/// have a name, but the more pressing fix anyway should be to focus
/// on the error that a module doesn't have a name.
pub(crate) fn item(
    db: &dyn Db,
    stmt: ast::Stmt,
    ast_locations: &AstLocations,
    loc_map: &mut ItemLocMap,
) -> Option<Vec<Item>> {
    Some(match stmt {
        ast::Stmt::ConstVarDecl(constvar) => {
            let names = constvar.constvar_names().unwrap();

            let items = names
                .names()
                .map(|decl_name| {
                    let loc = ast_locations.get(&decl_name);
                    let name = decl_name.name().unwrap().identifier_token().unwrap();
                    let name = Symbol::new(db, name.text().to_owned());
                    let item = ConstVar::new(db, name, ConstVarOrigin { loc });
                    loc_map.insert(loc, item);

                    Item::ConstVar(item)
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
            let item = Module::new(db, name, ModuleOrigin::Item(loc));
            loc_map.insert(loc, item);

            vec![Item::Module(item)]
        }
        // ast::Stmt::ClassDecl(_) => todo!(),
        // ast::Stmt::MonitorDecl(_) => todo!(),
        // ast::Stmt::ImportStmt(_) => todo!(),
        // ast::Stmt::ExportStmt(_) => todo!(),
        // ast::Stmt::PreprocGlob(_) => todo!(),
        _ => return None,
    })
}
