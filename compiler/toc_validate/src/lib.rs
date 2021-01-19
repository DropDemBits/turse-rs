//! AST validation
//! Checking if things hold up to stricter syntax semantics
// fancy quotes: ‘’
#[cfg(test)]
mod test;

use toc_reporting::{MessageKind, MessageSink, ReportMessage, TextRange};
use toc_syntax::{
    ast::{self, AstNode},
    SyntaxKind, SyntaxNode,
};

// Taken from rust-analyzer's syntax crate
macro_rules! match_ast {
    (match $node:ident { $($tt:tt)* }) => { match_ast!(match ($node) { $($tt)* }) };

    (match ($node:expr) {
        $( ast::$ast:ident($it:ident) => $res:expr, )*
        _ => $catch_all:expr $(,)?
    }) => {{
        $( if let Some($it) = ast::$ast::cast($node.clone()) { $res } else )*
        { $catch_all }
    }};
}

pub struct ValidateResult {
    messages: Vec<ReportMessage>,
}

impl ValidateResult {
    pub fn messages(&self) -> &[ReportMessage] {
        &self.messages
    }
}

pub fn validate_ast(root: SyntaxNode) -> ValidateResult {
    let mut ctx = ValidateCtx {
        sink: MessageSink::new(),
    };

    if let Some(source) = ast::Source::cast(root) {
        validate_source(source, &mut ctx);
    }

    ctx.finish()
}
struct ValidateCtx {
    sink: MessageSink,
}

impl ValidateCtx {
    pub(crate) fn push_error(&mut self, msg: &str, range: TextRange) {
        self.sink.report(MessageKind::Error, msg, range);
    }

    fn finish(self) -> ValidateResult {
        ValidateResult {
            messages: self.sink.finish(),
        }
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
                // ???: Should we report all stmts following unit declaration?
                ctx.push_error(
                    "found extra text after unit declaration",
                    after.syntax().text_range(),
                );
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
            ast::PreprocGlob(pp_glob) => validate_preproc_glob(pp_glob, ctx),
            ast::ConstVarDecl(decl) => validate_constvar_decl(decl, ctx),
            ast::ModuleDecl(decl) => validate_module_decl(decl, ctx),
            ast::ClassDecl(decl) => validate_class_decl(decl, ctx),
            ast::MonitorDecl(decl) => validate_monitor_decl(decl, ctx),
            ast::ElseStmt(stmt) => validate_else_stmt(stmt, ctx),
            ast::ElseifStmt(stmt) => validate_elseif_stmt(stmt, ctx),
            _ => (),
        })
    }
}

fn validate_preproc_glob(glob: ast::PreprocGlob, ctx: &mut ValidateCtx) {
    let directive = glob.directive().unwrap();
    match_ast!(match (directive.syntax()) {
        ast::PPElseif(nd) => without_matching(nd.syntax(), "#if", ctx),
        ast::PPElse(nd) => without_matching(nd.syntax(), "#if", ctx),
        ast::PPEndIf(nd) => without_matching(nd.syntax(), "#if", ctx),
        _ => (),
    });
}

fn validate_constvar_decl(decl: ast::ConstVarDecl, ctx: &mut ValidateCtx) {
    if let Some(attr) = decl.register_attr() {
        // 'register' attr is not allowed in top-level blocks:

        if block_containing_node(decl.syntax()).is_top_level() {
            ctx.push_error(
                "‘register’ attribute is not allowed at module level",
                attr.syntax().text_range(),
            );
        }
    }

    // TODO: stuff relating to init expr
}

fn validate_module_decl(decl: ast::ModuleDecl, ctx: &mut ValidateCtx) {
    // Check contained in location
    if !block_containing_node(decl.syntax()).is_top_level() {
        ctx.push_error(
            "modules can only be declared at the program, module, or monitor level",
            decl.module_token().unwrap().text_range(),
        );
    }

    check_matching_names(decl.name(), decl.end_group(), ctx);
}

fn validate_class_decl(decl: ast::ClassDecl, ctx: &mut ValidateCtx) {
    let is_monitor_class = decl.monitor_token().is_some();

    if let Some(dev_spec) = decl.device_spec() {
        assert!(is_monitor_class, "non-monitor class has device spec");

        ctx.push_error(
            "device specification is not allowed for monitor classes",
            dev_spec.syntax().text_range(),
        )
    }

    // Check contained in location
    if is_monitor_class {
        // specialize for monitor classes
        match block_containing_node(decl.syntax()) {
            block if block.is_monitor() => {
                ctx.push_error(
                    "monitor classes cannot be declared inside of monitors",
                    decl.class_token().unwrap().text_range(),
                );
            }
            BlockKind::Class => {
                ctx.push_error(
                    "monitor classes cannot be declared inside of classes",
                    decl.class_token().unwrap().text_range(),
                );
            }
            block if !block.is_top_level() => {
                ctx.push_error(
                    "monitor classes can only be declared at the program, module, or monitor level",
                    decl.class_token().unwrap().text_range(),
                );
            }
            _ => {}
        }
    } else {
        match block_containing_node(decl.syntax()) {
            block if block.is_monitor() => {
                ctx.push_error(
                    "classes cannot be declared inside of monitors",
                    decl.class_token().unwrap().text_range(),
                );
            }
            BlockKind::Class => {
                ctx.push_error(
                    "classes cannot be declared inside of other classes",
                    decl.class_token().unwrap().text_range(),
                );
            }
            block if !block.is_top_level() => {
                ctx.push_error(
                    "classes can only be declared at the program, module, or monitor level",
                    decl.class_token().unwrap().text_range(),
                );
            }
            _ => {}
        }
    }

    check_matching_names(decl.name(), decl.end_group(), ctx);
}

fn validate_monitor_decl(decl: ast::MonitorDecl, ctx: &mut ValidateCtx) {
    // Check contained in location
    match block_containing_node(decl.syntax()) {
        block if block.is_monitor() => {
            ctx.push_error(
                "monitors cannot be declared inside of other monitors",
                decl.monitor_token().unwrap().text_range(),
            );
        }
        block if !block.is_top_level() => {
            ctx.push_error(
                "monitors can only be declared at the program, module, or monitor level",
                decl.monitor_token().unwrap().text_range(),
            );
        }
        _ => {}
    }

    check_matching_names(decl.name(), decl.end_group(), ctx);
}

fn check_matching_names(
    decl_name: Option<ast::Name>,
    end_group: Option<ast::EndGroup>,
    ctx: &mut ValidateCtx,
) {
    if let Some(decl_name) = decl_name.and_then(|end| end.identifier_token()) {
        if let Some(end_name) = end_group.and_then(|end| end.identifier_token()) {
            if end_name.text() != decl_name.text() {
                ctx.push_error(
                    &format!(
                        "end identifier ‘{}’ does not match ‘{}’",
                        end_name.text(),
                        decl_name.text()
                    ),
                    end_name.text_range(),
                )
                // TODO: add additional diagnostic referring to the declared identifier
            }
        }
    }
}

fn validate_else_stmt(stmt: ast::ElseStmt, ctx: &mut ValidateCtx) {
    let parent_kind = stmt.syntax().parent().map(|p| p.kind());

    if !matches!(parent_kind, Some(SyntaxKind::IfBody)) {
        without_matching(stmt.syntax(), "if", ctx);
    }
}

fn validate_elseif_stmt(stmt: ast::ElseifStmt, ctx: &mut ValidateCtx) {
    let parent_kind = stmt.syntax().parent().map(|p| p.kind());

    if !matches!(parent_kind, Some(SyntaxKind::IfBody)) {
        without_matching(stmt.syntax(), "if", ctx);
    }
}

fn without_matching(node: &SyntaxNode, thing: &str, ctx: &mut ValidateCtx) {
    let first = node.first_token().unwrap();
    ctx.push_error(
        &format!("found ‘{}’ without matching ‘{}’", first.text(), thing),
        first.text_range(),
    );
}

#[cfg(test)]
#[track_caller]
pub(crate) fn check(source: &str, expected: expect_test::Expect) {
    let res = toc_parser::parse(source);
    let validate_res = validate_ast(res.syntax());

    let mut buf = String::new();
    for msg in res.messages().iter().chain(validate_res.messages().iter()) {
        buf.push_str(&format!("{}\n", msg));
    }
    let trimmed = buf.trim_end();

    expected.assert_eq(trimmed);
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum BlockKind {
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
        match self {
            Self::Main
            | Self::Unit
            | Self::Module
            | Self::Class
            | Self::Monitor
            | Self::MonitorClass
            | Self::MonitorDevice => true,
            _ => false,
        }
    }

    fn is_monitor(self) -> bool {
        matches!(
            self,
            BlockKind::Monitor | BlockKind::MonitorClass | BlockKind::MonitorDevice
        )
    }
}

fn block_containing_node(start: &SyntaxNode) -> BlockKind {
    walk_blocks(start).nth(0).unwrap()
}

fn walk_blocks(start: &SyntaxNode) -> impl Iterator<Item = BlockKind> {
    // walk up parents
    let mut parent = start.parent();

    std::iter::from_fn(move || {
        let kind = loop {
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

            parent = parent.as_ref()?.parent();
        };

        Some(kind)
    })
}
