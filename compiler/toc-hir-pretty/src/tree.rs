//! Pretty-printing HIR trees, in line-based output

use std::{
    cell::{Cell, RefCell},
    fmt::{self, Write},
    ops::DerefMut,
};

use toc_hir::{
    body,
    expr::{self, BodyExpr},
    item,
    package::{self, LoweredPackage},
    span::{HasSpanTable, SpanId, Spanned},
    stmt::{self, BodyStmt},
    symbol::{self, LocalDefId, Mutability, SubprogramKind},
    ty,
    visitor::{HirVisitor, WalkEvent, Walker},
};

pub fn pretty_print_tree(db: &dyn toc_paths::Db, lowered: &LoweredPackage) -> String {
    let mut output = String::new();
    let mut walker = Walker::from_package(lowered);

    let pretty = PrettyVisitor::new(db, &mut output, lowered);

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
    package: &'hir package::Package,
    db: &'hir dyn toc_paths::Db,
}

impl<'out, 'hir> PrettyVisitor<'out, 'hir> {
    fn new(
        db: &'hir dyn toc_paths::Db,
        out: &'out mut dyn fmt::Write,
        package: &'hir package::Package,
    ) -> Self {
        Self {
            out: RefCell::new(out),
            indent_level: Cell::new(0),
            package,
            db,
        }
    }

    fn emit_node(&self, name: &str, span: SpanId, extra: Option<fmt::Arguments<'_>>) {
        self.try_emit_node(name, span, extra)
            .expect("failed to emit node");
    }

    fn try_emit_node(
        &self,
        name: &str,
        span: SpanId,
        extra: Option<fmt::Arguments<'_>>,
    ) -> fmt::Result {
        let mut out = self.out.borrow_mut();

        self.emit_indent(out.deref_mut())?;

        // Tree format is:
        // $node_name@($file_id, $span): $additional_info
        write!(out, "{}@{}", name, self.display_span(span))?;

        if let Some(extra) = extra {
            write!(out, ": {extra}")?;
        }

        writeln!(out)
    }

    fn emit_indent(&self, out: &mut dyn fmt::Write) -> fmt::Result {
        for _ in 0..self.indent_level.get() {
            write!(out, "  ")?;
        }

        Ok(())
    }

    fn display_span(&self, span: SpanId) -> String {
        let span = span.lookup_in(self.package);

        if let Some((file_id, range)) = span.into_parts() {
            let file = file_id.into_raw().raw_path(self.db);
            let (start, end) = (u32::from(range.start()), u32::from(range.end()));

            format!("({file}:{start}..{end})")
        } else {
            "(dummy)".to_string()
        }
    }

    fn display_def(&self, def_id: LocalDefId) -> String {
        let def_info = &self.package.local_def(def_id);
        let name = def_info.name;
        let def_span = self.display_span(def_info.def_at);
        format!("{name:?}@{def_span}")
    }

    // TODO: fold the uses
    fn display_extra_def(&self, def_id: LocalDefId) -> String {
        self.display_def(def_id)
    }

    fn display_extra_binding(&self, binding: Spanned<symbol::Symbol>) -> String {
        match self.package.binding_resolve(binding) {
            symbol::Resolve::Def(def_id) => {
                let def_info = self.package.local_def(def_id);
                let name = def_info.name;
                let def_span = self.display_span(def_info.def_at);

                format!("{name:?}@{def_span}")
            }
            symbol::Resolve::Err => {
                let name = binding.item();
                let bind_span = self.display_span(binding.span());

                format!("{name:?}@{bind_span}, undeclared")
            }
        }
    }

    fn display_extra_def_resolve(&self, local_def: symbol::LocalDefId) -> String {
        match self.package.def_resolve(local_def) {
            symbol::DefResolve::Local(local_def) => {
                format!("local({})", self.display_def(local_def))
            }
            symbol::DefResolve::External(def) => {
                format!("external({def:?})")
            }
            symbol::DefResolve::Err => "unresolved".to_string(),
            symbol::DefResolve::Canonical => "".to_string(),
        }
    }

    fn display_param_info(&self, info: &ty::Parameter) -> String {
        let mut extra = String::new();

        match info.pass_by {
            ty::PassBy::Value => {}
            ty::PassBy::Reference(Mutability::Const) => write!(extra, "const ").unwrap(),
            ty::PassBy::Reference(Mutability::Var) => write!(extra, "var ").unwrap(),
        }

        if info.is_register {
            write!(extra, "register ").unwrap()
        }

        if info.coerced_type {
            write!(extra, "cheat ").unwrap()
        }

        extra
    }

    fn item_span(&self, id: item::ItemId) -> SpanId {
        self.package.item(id).span
    }

    fn body_span(&self, id: body::BodyId) -> SpanId {
        self.package.body(id).span
    }

    fn stmt_span(&self, id: BodyStmt) -> SpanId {
        self.package.body(id.0).stmt(id.1).span
    }

    fn expr_span(&self, id: BodyExpr) -> SpanId {
        self.package.body(id.0).expr(id.1).span
    }

    fn type_span(&self, id: ty::TypeId) -> SpanId {
        self.package.lookup_type(id).span
    }

    fn def_of(&self, item: item::ItemId) -> LocalDefId {
        self.package.item(item).def_id
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
    fn visit_package(&self, _package: &package::Package) {
        self.emit_node("Package", self.package.span_table().dummy_span(), None);
    }

    fn visit_file_root(&self, file: toc_span::FileId, id: item::ItemId) {
        let file = file.into_raw().raw_path(self.db);
        self.emit_node(
            "Root",
            self.package.span_table().dummy_span(),
            Some(format_args!("{file} -> {id:?}")),
        );
    }

    // Items //
    fn visit_constvar(&self, id: item::ItemId, item: &item::ConstVar) {
        let span = self.item_span(id);
        let def_id = self.def_of(id);

        let mut extra = String::new();

        match item.mutability {
            Mutability::Var => write!(extra, "var "),
            Mutability::Const => write!(extra, "const "),
        }
        .unwrap();

        if item.is_register {
            write!(extra, "register ").unwrap();
        }

        write!(extra, "{}", self.display_def(def_id)).unwrap();

        self.emit_node("ConstVar", span, Some(format_args!("{extra}")))
    }
    fn visit_type_decl(&self, id: item::ItemId, item: &item::Type) {
        let span = self.item_span(id);
        let def_id = self.def_of(id);

        let mut extra = String::new();

        if let item::DefinedType::Forward(_) = item.type_def {
            write!(extra, "forward ").unwrap()
        }

        write!(extra, "{}", self.display_def(def_id)).unwrap();

        self.emit_node("Type", span, Some(format_args!("{extra}")));
    }
    fn visit_bind_decl(&self, id: item::ItemId, item: &item::Binding) {
        let span = self.item_span(id);
        let def_id = self.def_of(id);

        let mut extra = String::new();

        match item.mutability {
            Mutability::Var => write!(extra, "var "),
            Mutability::Const => write!(extra, "const "),
        }
        .unwrap();

        if item.is_register {
            write!(extra, "register ").unwrap();
        }

        write!(extra, "{}", self.display_def(def_id)).unwrap();

        self.emit_node("Bind", span, Some(format_args!("{extra}")))
    }
    fn visit_subprogram_decl(&self, id: item::ItemId, item: &item::Subprogram) {
        let span = self.item_span(id);
        let def_id = self.def_of(id);

        let mut extra = String::new();

        write!(extra, "{}", self.display_def(def_id)).unwrap();

        if let Some(params) = &item.param_list {
            let param_count = params.tys.len();
            let mut params = params.names.iter().zip(params.tys.iter());

            write!(extra, " [").unwrap();
            if param_count > 1 {
                self.indent();
                writeln!(extra).unwrap();
                self.emit_indent(&mut extra).unwrap();
            }

            if let Some(first) = params.next() {
                write!(extra, "{}", self.display_param_info(first.1)).unwrap();
                write!(extra, "{}", self.display_def(*first.0)).unwrap();
            }
            for rest in params {
                writeln!(extra, ",").unwrap();
                self.emit_indent(&mut extra).unwrap();
                write!(extra, "{}", self.display_param_info(rest.1)).unwrap();
                write!(extra, "{}", self.display_def(*rest.0)).unwrap();
            }

            if param_count > 1 {
                self.unindent();
                writeln!(extra).unwrap();
                self.emit_indent(&mut extra).unwrap();
            }
            write!(extra, "]").unwrap();
        }

        if let Some(result) = item.result.name {
            write!(extra, " -> {}", self.display_def(result)).unwrap();
        }

        self.emit_node("Subprogram", span, Some(format_args!("{extra}")))
    }
    fn visit_module(&self, id: item::ItemId, item: &item::Module) {
        let span = self.item_span(id);
        let def_id = self.def_of(id);
        let extra = {
            let mut extra = String::new();
            write!(extra, "{}", self.display_def(def_id)).unwrap();

            // FIXME: Change to be the same format as import list
            if !item.exports.is_empty() {
                let mut exports = item.exports.iter();
                write!(extra, ", exports [").unwrap();

                let emit_export = |extra: &mut String, export: &item::ExportItem| {
                    write!(extra, "{:?} {:?}", export.mutability, export.qualify_as).unwrap();
                    if export.is_opaque {
                        write!(extra, " opaque").unwrap();
                    }
                    write!(extra, " {}", self.display_extra_def_resolve(export.def_id)).unwrap();
                };

                if let Some(first) = exports.next() {
                    emit_export(&mut extra, first);
                }
                for rest in exports {
                    write!(extra, ", ").unwrap();
                    emit_export(&mut extra, rest)
                }

                write!(extra, "]").unwrap();
            }

            extra
        };
        self.emit_node("Module", span, Some(format_args!("{extra}")))
    }
    fn visit_import(&self, id: item::ItemId, item: &item::Import) {
        let span = self.item_span(id);

        self.emit_node(
            "Import",
            span,
            Some(format_args!(
                "{:?} {}",
                item.mutability,
                self.display_extra_def_resolve(item.def_id)
            )),
        )
    }
    // Body
    fn visit_body(&self, id: body::BodyId, body: &body::Body) {
        let span = self.body_span(id);
        match &body.kind {
            body::BodyKind::Stmts(_, params, result_name) => {
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

                if let Some(result) = result_name {
                    write!(extra, " -> {}", self.display_def(*result)).unwrap();
                }

                self.emit_node("StmtBody", span, Some(format_args!("{extra}")))
            }
            body::BodyKind::Exprs(_) => self.emit_node("ExprBody", span, None),
        }
    }
    // Stmts //
    fn visit_item_stmt(&self, id: BodyStmt, item: item::ItemId) {
        let span = self.stmt_span(id);
        self.emit_node("StmtItem", span, Some(format_args!("{item:?}")))
    }
    fn visit_assign(&self, id: BodyStmt, _stmt: &stmt::Assign) {
        let span = self.stmt_span(id);
        self.emit_node("Assign", span, None)
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
    fn visit_for(&self, id: BodyStmt, stmt: &stmt::For) {
        let span = self.stmt_span(id);
        let bounds_kind = match stmt.bounds {
            stmt::ForBounds::Implicit(_) => "implicit",
            stmt::ForBounds::Full(_, _) => "explicit",
        };

        if stmt.is_decreasing {
            self.emit_node("For", span, Some(format_args!("decreasing {bounds_kind}")))
        } else {
            self.emit_node("For", span, Some(format_args!("{bounds_kind}")))
        }
    }
    fn visit_loop(&self, id: BodyStmt, _stmt: &stmt::Loop) {
        let span = self.stmt_span(id);
        self.emit_node("Loop", span, None)
    }
    fn visit_exit(&self, id: BodyStmt, _stmt: &stmt::Exit) {
        let span = self.stmt_span(id);
        self.emit_node("Exit", span, None)
    }
    fn visit_if(&self, id: BodyStmt, _stmt: &stmt::If) {
        let span = self.stmt_span(id);
        self.emit_node("If", span, None)
    }
    fn visit_case(&self, id: BodyStmt, _stmt: &stmt::Case) {
        let span = self.stmt_span(id);
        self.emit_node("Case", span, None)
    }
    fn visit_block(&self, id: BodyStmt, stmt: &stmt::Block) {
        let span = self.stmt_span(id);
        self.emit_node("Block", span, Some(format_args!("{:?}", stmt.kind)))
    }
    fn visit_call_stmt(&self, id: BodyStmt, stmt: &stmt::Call) {
        let span = self.stmt_span(id);
        let extra = match &stmt.arguments {
            Some(_) => "...",
            None => "no params",
        };
        self.emit_node("CallStmt", span, Some(format_args!("[{extra}]")));
    }
    fn visit_return_stmt(&self, id: BodyStmt, _stmt: &stmt::Return) {
        let span = self.stmt_span(id);
        self.emit_node("Return", span, None)
    }
    fn visit_result_stmt(&self, id: BodyStmt, _stmt: &stmt::Result) {
        let span = self.stmt_span(id);
        self.emit_node("Result", span, None)
    }

    // Exprs //
    fn visit_literal(&self, id: BodyExpr, expr: &expr::Literal) {
        let span = self.expr_span(id);
        self.emit_node("Literal", span, Some(format_args!("{expr:?}")))
    }
    fn visit_init_expr(&self, id: BodyExpr, _expr: &expr::Init) {
        let span = self.expr_span(id);
        self.emit_node("InitExpr", span, None);
    }
    fn visit_binary(&self, id: BodyExpr, expr: &expr::Binary) {
        let span = self.expr_span(id);
        self.emit_node("Binary", span, Some(format_args!("{:?}", expr.op.item())))
    }
    fn visit_unary(&self, id: BodyExpr, expr: &expr::Unary) {
        let span = self.expr_span(id);
        self.emit_node("Unary", span, Some(format_args!("{:?}", expr.op.item())))
    }
    fn visit_all_expr(&self, id: BodyExpr) {
        let span = self.expr_span(id);
        self.emit_node("All", span, None)
    }
    fn visit_range_expr(&self, id: BodyExpr, expr: &expr::Range) {
        let span = self.expr_span(id);
        let extra = {
            let mut extra = String::new();
            write!(extra, "{start:?}", start = expr.start).unwrap();
            if let Some(end) = expr.end {
                write!(extra, " .. {end:?}").unwrap();
            }
            extra
        };
        self.emit_node("Range", span, Some(format_args!("{extra}")))
    }
    fn visit_name(&self, id: BodyExpr, expr: &expr::Name) {
        let span = self.expr_span(id);
        match expr {
            expr::Name::Name(binding) => {
                let extra = self.display_extra_binding(*binding);
                self.emit_node("Name", span, Some(format_args!("{extra}")))
            }
            expr::Name::Self_ => self.emit_node("Self", span, None),
        }
    }
    fn visit_field(&self, id: BodyExpr, expr: &expr::Field) {
        let span = self.expr_span(id);
        self.emit_node(
            "Field",
            span,
            Some(format_args!("field {:?}", expr.field.item())),
        )
    }
    fn visit_deref(&self, id: BodyExpr, _expr: &expr::Deref) {
        let span = self.expr_span(id);
        self.emit_node("Deref", span, None)
    }
    fn visit_call_expr(&self, id: BodyExpr, _expr: &expr::Call) {
        let span = self.expr_span(id);
        self.emit_node("CallExpr", span, Some(format_args!("[...]")));
    }

    // Types //
    fn visit_primitive(&self, id: ty::TypeId, ty: &ty::Primitive) {
        let span = self.type_span(id);
        self.emit_node("Primitive", span, Some(format_args!("{ty:?}")))
    }
    fn visit_alias(&self, id: ty::TypeId, ty: &ty::Alias) {
        let span = self.type_span(id);
        let extra = self.display_extra_binding(ty.base_def);
        let segments = ty
            .segments
            .iter()
            .flat_map(|s| vec![".", s.item().name()])
            .collect::<String>();
        self.emit_node("Alias", span, Some(format_args!("{extra}{segments}")))
    }
    fn visit_constrained(&self, id: ty::TypeId, ty: &ty::Constrained) {
        let span = self.type_span(id);
        let extra = match ty.end {
            ty::ConstrainedEnd::Expr(_) => "end => Expr".into(),
            ty::ConstrainedEnd::Unsized(sz) => format!("end => Unsized({sz:?})", sz = sz.item()),
            ty::ConstrainedEnd::Any(_) => "end => Any".into(),
        };
        self.emit_node("Constrained", span, Some(format_args!("{extra}")))
    }
    fn visit_enum(&self, id: ty::TypeId, ty: &ty::Enum) {
        let span = self.type_span(id);
        let def_name = self.display_extra_def(ty.def_id);
        let variants = itertools::intersperse(
            ty.variants
                .iter()
                .map(|&variant| self.package.local_def(variant).name.name()),
            ",",
        )
        .collect::<String>();
        self.emit_node(
            "Enum",
            span,
            Some(format_args!("{def_name} [ {variants} ]")),
        )
    }
    fn visit_array(&self, id: ty::TypeId, ty: &ty::Array) {
        let span = self.type_span(id);
        self.emit_node("Array", span, Some(format_args!("{:?}", ty.sizing)))
    }
    fn visit_set(&self, id: ty::TypeId, ty: &ty::Set) {
        let span = self.type_span(id);
        let extra = self.display_extra_def(ty.def_id);
        self.emit_node("Set", span, Some(format_args!("{extra}")));
    }
    fn visit_pointer(&self, id: ty::TypeId, ty: &ty::Pointer) {
        let span = self.type_span(id);
        self.emit_node(
            "Pointer",
            span,
            Some(format_args!("{extra:?}", extra = ty.checked)),
        );
    }
    fn visit_subprogram_ty(&self, id: ty::TypeId, ty: &ty::Subprogram) {
        let span = self.type_span(id);
        let name = match ty.kind {
            SubprogramKind::Procedure => "Procedure",
            SubprogramKind::Function => "Function",
            SubprogramKind::Process => "Process",
        };
        let extra = match &ty.param_list {
            Some(_) => "...",
            None => "no params",
        };

        self.emit_node(name, span, Some(format_args!("[{extra}]")));
    }
    fn visit_void(&self, id: ty::TypeId) {
        let span = self.type_span(id);
        self.emit_node("Void", span, None)
    }
}
