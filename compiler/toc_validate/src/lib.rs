//! AST validation
//! Checking if things hold up to stricter syntax semantics
// fancy quotes: ‘’
mod preproc;
mod stmt;
#[cfg(test)]
mod test;

use toc_reporting::{CompileResult, MessageBuilder, MessageSink};
use toc_span::{FileId, Span, TextRange};
use toc_syntax::{
    ast::{self, AstNode},
    match_ast, SyntaxNode,
};

pub fn validate_ast(file: Option<FileId>, root: SyntaxNode) -> CompileResult<()> {
    let mut ctx = ValidateCtx {
        file,
        sink: MessageSink::new(),
    };

    if let Some(source) = ast::Source::cast(root) {
        validate_source(source, &mut ctx);
    }

    ctx.finish()
}
struct ValidateCtx {
    file: Option<FileId>,
    sink: MessageSink,
}

impl ValidateCtx {
    pub(crate) fn push_error(&mut self, msg: &str, range: TextRange) {
        self.sink.error(msg, Span::new(self.file, range));
    }

    pub(crate) fn push_detailed_error(&mut self, msg: &str, range: TextRange) -> MessageBuilder {
        self.sink.error_detailed(msg, Span::new(self.file, range))
    }

    fn finish(self) -> CompileResult<()> {
        CompileResult::new((), self.sink.finish())
    }
}

fn validate_source(src: ast::Source, ctx: &mut ValidateCtx) {
    let root = src.syntax();

    if let Some(unit) = src.unit_token() {
        // as unit, special considerations for allowed statements
        let stmt_list = src.stmt_list().unwrap();
        let mut child_stmts = stmt_list.stmts();

        if let Some(node) = child_stmts.next() {
            match_ast!(match (node.syntax()) {
                ast::ModuleDecl(_decl) => (),
                ast::ClassDecl(_decl) => (),
                ast::MonitorDecl(_decl) => (),
                _ => {
                    ctx.push_error(
                        "expected a module, class, or monitor declaration",
                        node.syntax().text_range(),
                    );
                }
            });

            if let Some(after) = child_stmts.next() {
                // Make report range covering all stmts following the unit declaration
                let start_range = after.syntax().text_range();
                let end_range = child_stmts
                    .last()
                    .map(|node| node.syntax().text_range())
                    .unwrap_or(start_range);
                let full_range = start_range.cover(end_range);

                ctx.push_error("found extra text after unit declaration", full_range);
            }
        } else {
            ctx.push_error(
                "expected a module, class, or monitor declaration",
                unit.text_range(),
            );
        }
    }

    // Indiscriminately go over descendant nodes
    for node in root.descendants() {
        match_ast!(match node {
            ast::PreprocGlob(pp_glob) => preproc::validate_preproc_glob(pp_glob, ctx),
            ast::ConstVarDecl(decl) => stmt::validate_constvar_decl(decl, ctx),
            ast::BindDecl(decl) => stmt::validate_bind_decl(decl, ctx),
            ast::ProcDecl(decl) =>
                stmt::validate_in_top_level(decl.syntax(), "‘procedure’ declaration", ctx),
            ast::ProcHeader(node) => stmt::validate_proc_header(node, ctx),
            ast::FcnDecl(decl) =>
                stmt::validate_in_top_level(decl.syntax(), "‘function’ declaration", ctx),
            ast::ProcessDecl(decl) => stmt::validate_process_decl(decl, ctx),
            ast::ExternalVar(var) => stmt::validate_external_var(var, ctx),
            ast::ForwardDecl(decl) =>
                stmt::validate_in_top_level(decl.syntax(), "‘forward’ declaration", ctx),
            ast::DeferredDecl(decl) => stmt::validate_deferred_decl(decl, ctx),
            ast::BodyDecl(decl) =>
                stmt::validate_in_top_level(decl.syntax(), "‘body’ declaration", ctx),
            ast::ModuleDecl(decl) => stmt::validate_module_decl(decl, ctx),
            ast::ClassDecl(decl) => stmt::validate_class_decl(decl, ctx),
            ast::MonitorDecl(decl) => stmt::validate_monitor_decl(decl, ctx),
            ast::NewOpen(open) => stmt::validate_new_open(open, ctx),
            ast::ForStmt(stmt) => stmt::validate_for_stmt(stmt, ctx),
            ast::ElseStmt(stmt) => stmt::validate_else_stmt(stmt, ctx),
            ast::ElseifStmt(stmt) => stmt::validate_elseif_stmt(stmt, ctx),
            ast::CaseStmt(stmt) => stmt::validate_case_stmt(stmt, ctx),
            ast::InvariantStmt(stmt) => stmt::validate_invariant_stmt(stmt, ctx),
            _ => (),
        })
    }
}

#[cfg(test)]
#[track_caller]
pub(crate) fn check(source: &str, expected: expect_test::Expect) {
    let res = toc_parser::parse(None, source);
    let validate_res = validate_ast(None, res.result().syntax());

    let mut buf = String::new();
    for msg in res.messages().iter().chain(validate_res.messages().iter()) {
        buf.push_str(&format!("{}\n", msg));
    }
    let trimmed = buf.trim_end();

    expected.assert_eq(trimmed);
}

pub(crate) fn without_matching(node: &SyntaxNode, thing: &str, ctx: &mut ValidateCtx) {
    let first = node.first_token().unwrap();
    ctx.push_error(
        &format!("found ‘{}’ without matching ‘{}’", first.text(), thing),
        first.text_range(),
    );
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub(crate) enum BlockKind {
    Main,
    Unit,
    Module,
    Class,
    Monitor,
    MonitorClass,
    MonitorDevice,
    Function,
    Procedure,
    Process,
    Body,
    Loop,
    Inner,
}

impl BlockKind {
    fn is_top_level(self) -> bool {
        matches!(
            self,
            Self::Main
                | Self::Unit
                | Self::Module
                | Self::Class
                | Self::Monitor
                | Self::MonitorClass
                | Self::MonitorDevice
        )
    }

    fn is_monitor(self) -> bool {
        matches!(
            self,
            BlockKind::Monitor | BlockKind::MonitorClass | BlockKind::MonitorDevice
        )
    }

    fn is_module_kind(self) -> bool {
        matches!(
            self,
            Self::Module | Self::Class | Self::Monitor | Self::MonitorClass | Self::MonitorDevice
        )
    }
}

pub(crate) fn block_containing_node(start: &SyntaxNode) -> BlockKind {
    walk_blocks(start).next().unwrap()
}

pub(crate) fn walk_blocks(start: &SyntaxNode) -> impl Iterator<Item = BlockKind> {
    // walk up parents
    let mut parent = Some(start.clone());

    std::iter::from_fn(move || {
        let kind = loop {
            parent = parent.as_ref().and_then(|n| n.parent());

            match_ast!(match (parent.as_ref()?) {
                ast::Source(src) =>
                    break if src.unit_token().is_some() {
                        BlockKind::Unit
                    } else {
                        BlockKind::Main
                    },
                ast::ModuleDecl(_m) => break BlockKind::Module,
                ast::MonitorDecl(mon) =>
                    break if mon.device_spec().is_some() {
                        BlockKind::MonitorDevice
                    } else {
                        BlockKind::Monitor
                    },
                ast::ClassDecl(clz) =>
                    break if clz.monitor_token().is_some() {
                        BlockKind::MonitorClass
                    } else {
                        BlockKind::Class
                    },
                ast::FcnDecl(_f) => break BlockKind::Function,
                ast::ProcDecl(_f) => break BlockKind::Procedure,
                ast::ProcessDecl(_f) => break BlockKind::Process,
                ast::BodyDecl(_o) => break BlockKind::Body,
                ast::ForStmt(_f) => break BlockKind::Loop,
                ast::LoopStmt(_f) => break BlockKind::Loop,
                ast::IfStmt(_o) => break BlockKind::Inner,
                ast::ElseifStmt(_o) => break BlockKind::Inner,
                ast::ElseStmt(_o) => break BlockKind::Inner,
                ast::CaseStmt(_o) => break BlockKind::Inner,
                ast::BlockStmt(_o) => break BlockKind::Inner,
                ast::HandlerStmt(_o) => break BlockKind::Inner,
                _ => (),
            });
        };

        Some(kind)
    })
}
