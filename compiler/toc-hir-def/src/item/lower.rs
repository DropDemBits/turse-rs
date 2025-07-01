//! Lowering from AST declaration nodes to semantic item representations

use toc_ast_db::ast_id::AstIdMap;
use toc_hir_expand::{SemanticFile, SemanticLoc, UnstableSemanticLoc};
use toc_syntax::ast;

use crate::{
    Db, Symbol,
    item::{ConstVar, Item, ItemCollection, Module},
};

/// Collects the immediately accessible items from a [`ast::StmtList`]
pub(crate) fn collect_items<'db>(
    db: &'db dyn Db,
    stmt_list: UnstableSemanticLoc<ast::StmtList>,
) -> ItemCollection<'db> {
    let file = stmt_list.file();
    let ast_id_map = file.ast_id_map(db);
    let stmt_list = stmt_list.to_node(db);

    let items = stmt_list
        .stmts()
        .filter_map(|stmt| item(db, stmt, file, ast_id_map))
        .flatten()
        .collect::<Vec<_>>();

    ItemCollection::new(db, items.into())
}

/// Lowers a potential item, and returns either the new item, or `None`
/// if we don't support lowering it yet or it's not a well-formed item
/// (mainly because it doesn't have a name).
///
/// Note that this means that we'll drop errors when a module doesn't
/// have a name, but the more pressing fix anyway should be to focus
/// on the error that a module doesn't have a name.
pub(crate) fn item<'db>(
    db: &'db dyn Db,
    stmt: ast::Stmt,
    file: SemanticFile,
    ast_id_map: &'db AstIdMap,
) -> Option<Vec<Item<'db>>> {
    Some(match stmt {
        ast::Stmt::ConstVarDecl(constvar) => {
            let names = constvar.constvar_names().unwrap();

            let mut items = vec![];

            for decl_name in names.names() {
                let Some(ast_id) = ast_id_map.lookup_for_maybe(&decl_name) else {
                    return None;
                };
                let ast_id = SemanticLoc::from_ast_id(db, file, ast_id);
                let name = decl_name.name().unwrap().identifier_token().unwrap();
                let name = Symbol::new(db, name.text().to_owned());
                let item = ConstVar::new(db, name, ast_id);

                items.push(Item::ConstVar(item));
            }

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
            let ast_id = ast_id_map.lookup(&module);
            let ast_id = SemanticLoc::from_ast_id(db, file, ast_id);
            let name = module.name()?.identifier_token().unwrap();
            let name = Symbol::new(db, name.text().to_owned());
            let item = Module::new(db, name, ast_id);

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
