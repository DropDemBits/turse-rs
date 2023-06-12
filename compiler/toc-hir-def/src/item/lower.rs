//! Lowering from AST declaration nodes to semantic item representations

use toc_hir_expand::{AstLocations, SemanticLoc};
use toc_syntax::ast;

use crate::{
    item::{Module, ModuleOrigin},
    Db, Symbol,
};

use super::{ConstVar, ConstVarOrigin, Item};

/// Collects the immediately accessible items from a [`ast::StmtList`]
pub(crate) fn collect_items(db: &dyn Db, stmt_list: SemanticLoc<ast::StmtList>) -> Vec<Item> {
    let ast_locations = stmt_list.file(db.up()).ast_locations(db.up());
    let stmt_list = stmt_list.to_node(db.up());

    stmt_list
        .stmts()
        .filter_map(|stmt| item(db, stmt, ast_locations))
        .flatten()
        .collect()
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
) -> Option<Vec<Item>> {
    Some(match stmt {
        ast::Stmt::ConstVarDecl(constvar) => {
            // uhhhhhhh
            let loc = ast_locations.get(&constvar);
            let names = constvar.decl_list().unwrap();
            let items = names
                .names()
                .enumerate()
                .map(|(index, name)| {
                    let name = Symbol::new(db, name.identifier_token().unwrap().text().to_owned());

                    Item::ConstVar(ConstVar::new(db, name, ConstVarOrigin { loc, index }))
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
            let name = module.name()?.identifier_token().unwrap();
            let name = Symbol::new(db, name.text().to_owned());

            vec![Item::Module(Module::new(
                db,
                name,
                ModuleOrigin::Item(ast_locations.get(&module)),
            ))]
        }
        // ast::Stmt::ClassDecl(_) => todo!(),
        // ast::Stmt::MonitorDecl(_) => todo!(),
        // ast::Stmt::ImportStmt(_) => todo!(),
        // ast::Stmt::ExportStmt(_) => todo!(),
        // ast::Stmt::PreprocGlob(_) => todo!(),
        _ => return None,
    })
}
