//! Pretty printer(s) for HIR trees

use std::{
    cell::{Cell, RefCell},
    fmt,
};

use toc_hir::{
    body, expr, item,
    library::{self, SpannedLibrary},
    stmt,
    symbol::LocalDefId,
    ty,
    visitor::{BodyExpr, BodyStmt, HirVisitor, WalkEvent, Walker},
};
use toc_span::{SpanId, SpanTable};

pub fn pretty_print_tree(ty_intern: &dyn ty::TypeInterner, lowered: &SpannedLibrary) -> String {
    let mut output = String::new();
    let mut walker = Walker::new(lowered.library(), ty_intern);

    let pretty = PrettyVisitor::new(
        &mut output,
        lowered.library(),
        lowered.span_map(),
        ty_intern,
    );

    while let Some(event) = walker.next_event() {
        match event {
            WalkEvent::Enter(node) => {
                node.visit_node(&pretty);
                pretty.indent();
            }
            WalkEvent::Leave(_node) => {
                pretty.unindent();
            }
        };
    }

    output
}

struct PrettyVisitor<'out, 'hir> {
    out: RefCell<&'out mut dyn fmt::Write>,
    indent_level: Cell<usize>,
    library: &'hir library::Library,
    span_map: &'hir SpanTable,
    ty_intern: &'hir dyn ty::TypeInterner,
}

impl<'out, 'hir> PrettyVisitor<'out, 'hir> {
    fn new(
        out: &'out mut dyn fmt::Write,
        library: &'hir library::Library,
        span_map: &'hir SpanTable,
        ty_intern: &'hir dyn ty::TypeInterner,
    ) -> Self {
        Self {
            out: RefCell::new(out),
            indent_level: Cell::new(0),
            library,
            span_map,
            ty_intern,
        }
    }

    fn emit_node<'a>(&self, name: &str, span: SpanId, extra: Option<fmt::Arguments<'a>>) {
        self.try_emit_node(name, span, extra)
            .expect("failed to emit node");
    }

    fn try_emit_node<'a>(
        &self,
        name: &str,
        span: SpanId,
        extra: Option<fmt::Arguments<'a>>,
    ) -> fmt::Result {
        let mut out = self.out.borrow_mut();

        // Emit indentation
        for _ in 0..self.indent_level.get() {
            write!(out, "  ")?;
        }

        // Tree format is:
        // $node_name@($file_id, $span): $additional_info
        write!(out, "{}@{}", name, self.display_span(span))?;

        if let Some(extra) = extra {
            write!(out, ": {}", extra)?;
        }

        writeln!(out)
    }

    fn display_span(&self, span: SpanId) -> String {
        let span = self.span_map.lookup_span(span);

        if let Some(file_id) = span.file {
            format!("({:?}, {:?})", file_id, span.range)
        } else {
            "(dummy)".to_string()
        }
    }

    fn display_def(&self, def_id: LocalDefId) -> String {
        let name = &self.library.local_def(def_id).name;
        let def_span = self.display_span(name.span());
        format!("{:?}@{}", name.item(), def_span)
    }

    fn item_span(&self, id: item::ItemId) -> SpanId {
        self.library.item(id).span
    }

    fn body_span(&self, id: body::BodyId) -> SpanId {
        self.library.body(id).span
    }

    fn stmt_span(&self, id: BodyStmt) -> SpanId {
        self.library.body(id.0).stmt(id.1).span
    }

    fn expr_span(&self, id: BodyExpr) -> SpanId {
        self.library.body(id.0).expr(id.1).span
    }

    fn type_span(&self, id: ty::TypeId) -> SpanId {
        self.ty_intern.lookup_type(id).span
    }

    fn def_of(&self, item: item::ItemId) -> LocalDefId {
        self.library.item(item).def_id
    }

    fn indent(&self) {
        // can simplify once Cell::update is stabilized
        let level = self.indent_level.get();
        self.indent_level.set(level.saturating_add(1));
    }

    fn unindent(&self) {
        // can simplify once Cell::update is stabilized
        let level = self.indent_level.get();
        self.indent_level.set(level.saturating_sub(1));
    }
}

impl<'out, 'hir> HirVisitor for PrettyVisitor<'out, 'hir> {
    fn visit_library(&self, _library: &library::Library) {
        self.emit_node("Library", self.span_map.dummy_span(), None);
    }

    fn visit_file_root(&self, file: toc_span::FileId, id: item::ItemId) {
        self.emit_node(
            "Root",
            self.span_map.dummy_span(),
            Some(format_args!("{:?} -> {:?}", file, id)),
        );
    }

    // Items //
    fn visit_constvar(&self, id: item::ItemId, item: &item::ConstVar) {
        use fmt::Write;

        let span = self.item_span(id);
        let def_id = self.def_of(id);

        let mut extra = String::new();

        match item.mutability {
            item::Mutability::Var => write!(extra, "var "),
            item::Mutability::Const => write!(extra, "const "),
        }
        .unwrap();

        if item.is_register {
            write!(extra, "register ").unwrap();
        }

        write!(extra, "{}", self.display_def(def_id)).unwrap();

        self.emit_node("ConstVar", span, Some(format_args!("{}", extra)))
    }
    fn visit_module(&self, id: item::ItemId, _item: &item::Module) {
        let span = self.item_span(id);
        let def_id = self.def_of(id);
        self.emit_node(
            "Module",
            span,
            Some(format_args!("{}", self.display_def(def_id))),
        )
    }
    // Body
    fn visit_body(&self, id: body::BodyId, body: &body::Body) {
        use fmt::Write;

        let span = self.body_span(id);
        match &body.kind {
            body::BodyKind::Stmts(_, params) => {
                let mut params = params.iter();
                let mut extra = String::new();
                write!(extra, "[").unwrap();
                if let Some(first) = params.next() {
                    write!(extra, "{}", self.display_def(*first)).unwrap();
                }
                for rest in params {
                    write!(extra, ", {}", self.display_def(*rest)).unwrap();
                }
                write!(extra, "]").unwrap();

                self.emit_node("StmtBody", span, Some(format_args!("{}", extra)))
            }
            body::BodyKind::Exprs(_) => self.emit_node("ExprBody", span, None),
        }
    }
    // Stmts //
    fn visit_item_stmt(&self, id: BodyStmt, item: item::ItemId) {
        let span = self.stmt_span(id);
        self.emit_node("StmtItem", span, Some(format_args!("{:?}", item)))
    }
    fn visit_assign(&self, id: BodyStmt, stmt: &stmt::Assign) {
        let span = self.stmt_span(id);
        self.emit_node("Assign", span, Some(format_args!("{:?}", stmt.op.item())))
    }
    fn visit_put(&self, id: BodyStmt, stmt: &stmt::Put) {
        let span = self.stmt_span(id);
        let extra = stmt.append_newline.then(|| format_args!("newline"));
        self.emit_node("Put", span, extra)
    }
    fn visit_get(&self, id: BodyStmt, _stmt: &stmt::Get) {
        let span = self.stmt_span(id);
        self.emit_node("Get", span, None)
    }
    fn visit_block(&self, id: BodyStmt, stmt: &stmt::Block) {
        let span = self.stmt_span(id);
        self.emit_node("Block", span, Some(format_args!("{:?}", stmt.kind)))
    }
    // Exprs //
    fn visit_literal(&self, id: BodyExpr, expr: &expr::Literal) {
        let span = self.expr_span(id);
        self.emit_node("Literal", span, Some(format_args!("{:?}", expr)))
    }
    fn visit_binary(&self, id: BodyExpr, expr: &expr::Binary) {
        let span = self.expr_span(id);
        self.emit_node("Binary", span, Some(format_args!("{:?}", expr.op.item())))
    }
    fn visit_unary(&self, id: BodyExpr, expr: &expr::Unary) {
        let span = self.expr_span(id);
        self.emit_node("Unary", span, Some(format_args!("{:?}", expr.op.item())))
    }
    fn visit_paren(&self, id: BodyExpr, _expr: &expr::Paren) {
        let span = self.expr_span(id);
        self.emit_node("Paren", span, None)
    }
    fn visit_name(&self, id: BodyExpr, expr: &expr::Name) {
        let span = self.expr_span(id);
        match expr {
            expr::Name::Name(def_id) => {
                let def_info = self.library.local_def(*def_id);
                let def_display = self.display_def(*def_id);
                let extra = if matches!(def_info.kind, toc_hir::symbol::SymbolKind::Undeclared) {
                    format!("{}, undeclared", def_display)
                } else {
                    def_display
                };

                self.emit_node("Name", span, Some(format_args!("{}", extra)))
            }
            expr::Name::Self_ => self.emit_node("Self", span, None),
        }
    }
    // Types //
    fn visit_primitive(&self, id: ty::TypeId, ty: &ty::Primitive) {
        let span = self.type_span(id);
        self.emit_node("Primitive", span, Some(format_args!("{:?}", ty)))
    }
}
