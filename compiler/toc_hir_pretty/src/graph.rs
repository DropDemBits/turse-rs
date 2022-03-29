//! Pretty-printing HIR trees, as a GraphViz dot file

use std::{
    cell::RefCell,
    fmt::{self, Write},
    ops::DerefMut,
};

use toc_hir::library::LibraryId;
use toc_hir::{
    body,
    expr::{self, BodyExpr},
    item,
    library::{self, LoweredLibrary},
    library_graph::LibraryGraph,
    stmt::{self, BodyStmt},
    symbol::{LocalDefId, Mutability, SubprogramKind},
    ty,
    visitor::{HirVisitor, WalkEvent, Walker},
};
use toc_span::SpanId;

const IS_LR_LAYOUT: bool = true;

pub fn pretty_print_graph(
    library_graph: LibraryGraph,
    get_library: impl Fn(LibraryId) -> LoweredLibrary,
) -> String {
    let mut output = String::new();
    writeln!(output, "digraph hir_graph {{").unwrap();
    writeln!(output, "node [shape=Mrecord]").unwrap();
    writeln!(
        output,
        "rankdir={rank}",
        rank = if IS_LR_LAYOUT { "LR" } else { "BT" }
    )
    .unwrap();

    // Define the contents of the libraries
    for (_, library_id) in library_graph.library_roots() {
        writeln!(output, r#"subgraph "cluster_{library_id:?}" {{"#).unwrap();
        writeln!(output, r#"label="{library_id:?}""#).unwrap();
        let library = get_library(library_id);
        let mut walker = Walker::from_library(&library);

        let pretty = PrettyVisitor::new(&mut output, library_id, &library);

        while let Some(event) = walker.next_event() {
            match event {
                WalkEvent::Enter(node) => {
                    node.visit_node(&pretty);
                }
                WalkEvent::Leave(_node) => {}
            };
        }
        writeln!(output, "}}").unwrap();
    }

    // ... then the connections between libraries
    // FIXME: walk along library graph dep edges

    writeln!(output, "}}").unwrap();
    output
}

enum Layout {
    Empty,
    Hbox(Vec<Layout>),
    Vbox(Vec<Layout>),
    Node(String),
    Port(String),
    NamedPort(String, String),
}

impl Layout {
    fn try_emit_layout(&self, out: &mut dyn fmt::Write, parent: Option<&'_ Layout>) -> fmt::Result {
        let flip_dir = matches!(
            (self, parent),
            (Self::Hbox(_), None)
                | (Self::Vbox(_), Some(Self::Hbox(_)))
                | (Self::Hbox(_), Some(Self::Vbox(_)))
        );

        match self {
            Layout::Empty => {}
            Layout::Hbox(layouts) | Layout::Vbox(layouts) if flip_dir => {
                let mut iter = layouts.iter();
                write!(out, "{{ ")?;
                if let Some(layout) = iter.next() {
                    layout.try_emit_layout(out, Some(self))?;
                }
                for layout in iter {
                    write!(out, " | ")?;
                    layout.try_emit_layout(out, Some(self))?;
                }
                write!(out, " }}")?;
            }
            Layout::Hbox(layouts) | Layout::Vbox(layouts) => {
                let mut iter = layouts.iter();
                if let Some(layout) = iter.next() {
                    layout.try_emit_layout(out, Some(self))?;
                }
                for layout in iter {
                    write!(out, " | ")?;
                    layout.try_emit_layout(out, Some(self))?;
                }
            }
            Layout::Node(s) => write!(out, "{s}")?,
            Layout::Port(s) => write!(out, "<{s}> {s}")?,
            Layout::NamedPort(port, s) => write!(out, "<{port}> {s}")?,
        }

        Ok(())
    }
}

struct PrettyVisitor<'out, 'hir> {
    out: RefCell<&'out mut dyn fmt::Write>,
    library: &'hir library::Library,
    library_id: LibraryId,
}

impl<'out, 'hir> PrettyVisitor<'out, 'hir> {
    fn new(
        out: &'out mut dyn fmt::Write,
        library_id: LibraryId,
        library: &'hir library::Library,
    ) -> Self {
        Self {
            out: RefCell::new(out),
            library_id,
            library,
        }
    }

    fn emit_node(&self, id: &str, name: &str, span: SpanId, layout: Layout) {
        self.try_emit_node(id, name, span, layout)
            .expect("failed to emit node");
    }

    fn try_emit_node(&self, id: &str, name: &str, span: SpanId, layout: Layout) -> fmt::Result {
        let mut out = self.out.borrow_mut();

        let (start, end) = if IS_LR_LAYOUT { ("", "") } else { ("{", "}") };

        // Graph format is:
        // $id [label="$node_name@($file_id, $span) | $layout"]
        write!(
            out,
            r#"{id} [label="{start}{name}@{span} | "#,
            span = self.display_span(span)
        )?;
        layout.try_emit_layout(out.deref_mut(), None)?;

        if matches!(layout, Layout::Empty) {
            // without extra separator
            writeln!(out, r#"{end}"]"#)?;
        } else {
            writeln!(out, r#" | {end}"]"#)?;
        }

        Ok(())
    }

    fn emit_edge(&self, from: impl Into<String>, to: impl Into<String>) {
        let mut out = self.out.borrow_mut();
        let from = from.into();
        let to = to.into();
        writeln!(
            &mut *out,
            r#"{from} -> {to} [edgetooltip="to {target}"]"#,
            target = to.escape_debug()
        )
        .expect("failed to emit edge");
    }

    fn display_span(&self, span: SpanId) -> String {
        let span = self.library.span_map.lookup_span(span);

        if let Some((file_id, range)) = span.into_parts() {
            format!("({file_id:?}, {range:?})")
        } else {
            "(dummy)".to_string()
        }
    }

    fn layout_param_info(&self, info: &ty::Parameter) -> Layout {
        let mut h_layout = vec![];

        let pass_by = match info.pass_by {
            ty::PassBy::Value => "Value".into(),
            ty::PassBy::Reference(Mutability::Const) => "Const".into(),
            ty::PassBy::Reference(Mutability::Var) => "Var".into(),
        };
        h_layout.push(Layout::Node(pass_by));

        if info.is_register {
            h_layout.push(Layout::Node("Register".into()))
        }

        if info.coerced_type {
            h_layout.push(Layout::Node("Cheat".into()))
        }

        Layout::Hbox(h_layout)
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
        self.library.lookup_type(id).span
    }

    fn item_id(&self, item_id: item::ItemId) -> String {
        format!(r#""{lib_id:?}:{item_id:?}""#, lib_id = self.library_id)
    }

    fn body_id(&self, body_id: body::BodyId) -> String {
        format!(r#""{lib_id:?}:{body_id:?}""#, lib_id = self.library_id)
    }

    fn stmt_id(&self, BodyStmt(body_id, stmt_id): BodyStmt) -> String {
        format!(
            r#""{lib_id:?}:{body_id:?}:{stmt_id:?}""#,
            lib_id = self.library_id
        )
    }

    fn expr_id(&self, BodyExpr(body_id, expr_id): BodyExpr) -> String {
        format!(
            r#""{lib_id:?}:{body_id:?}:{expr_id:?}""#,
            lib_id = self.library_id
        )
    }

    fn def_id(&self, def_id: LocalDefId) -> String {
        format!(r#""{lib_id:?}:{def_id:?}""#, lib_id = self.library_id)
    }

    fn type_id(&self, type_id: ty::TypeId) -> String {
        format!(r#""{lib_id:?}:{type_id:?}""#, lib_id = self.library_id)
    }

    fn emit_item(&self, id: item::ItemId, name: &str, layout: Layout) {
        self.emit_node(&self.item_id(id), name, self.item_span(id), layout);
    }

    fn emit_body(&self, id: body::BodyId, name: &str, layout: Layout) {
        self.emit_node(&self.body_id(id), name, self.body_span(id), layout);
    }

    fn emit_stmt(&self, id: BodyStmt, name: &str, layout: Layout) {
        self.emit_node(&self.stmt_id(id), name, self.stmt_span(id), layout);
    }

    fn emit_expr(&self, id: BodyExpr, name: &str, layout: Layout) {
        self.emit_node(&self.expr_id(id), name, self.expr_span(id), layout);
    }

    fn emit_type(&self, id: ty::TypeId, name: &str, layout: Layout) {
        self.emit_node(&self.type_id(id), name, self.type_span(id), layout);
    }

    fn emit_def_id(&self, def_id: LocalDefId) {
        let def_info = self.library.local_def(def_id);

        self.emit_node(
            &self.def_id(def_id),
            "DefInfo",
            def_info.name.span(),
            Layout::Vbox(vec![
                Layout::Node(format!("name: '{name}'", name = def_info.name.item())),
                Layout::Node(format!("kind: {kind:?}", kind = def_info.kind)),
            ]),
        )
    }

    fn emit_linked_stmts(
        &self,
        from_node: &str,
        from_port: &str,
        body_id: body::BodyId,
        stmts: &[stmt::StmtId],
    ) {
        let body = self.library.body(body_id);
        for stmt in stmts {
            // short-circuit directly to the item
            let to = if let stmt::StmtKind::Item(item_id) = &body.stmt(*stmt).kind {
                self.item_id(*item_id)
            } else {
                self.stmt_id(BodyStmt(body_id, *stmt))
            };
            self.emit_edge(format!("{from_node}:{from_port}"), to);
        }
    }
}

impl<'out, 'hir> HirVisitor for PrettyVisitor<'out, 'hir> {
    fn visit_library(&self, library: &library::Library) {
        // Deal with all of the undeclared defs
        for def_id in library.local_defs() {
            let def_info = library.local_def(def_id);
            if !matches!(def_info.kind, toc_hir::symbol::SymbolKind::Undeclared) {
                continue;
            }

            self.emit_def_id(def_id);
        }
    }

    // Items //
    fn visit_constvar(&self, id: item::ItemId, item: &item::ConstVar) {
        self.emit_item(
            id,
            "ConstVar",
            Layout::Vbox(vec![
                Layout::Port("def_id".into()),
                Layout::Hbox(vec![
                    Layout::Node(format!("{muta:?}", muta = item.mutability)),
                    Layout::Node(format!("register: {reg:?}", reg = item.is_register)),
                ]),
                Layout::Port("type_spec".into()),
                Layout::Port("init_expr".into()),
            ]),
        );
        self.emit_def_id(item.def_id);

        // def_id
        self.emit_edge(
            format!("{item_id}:def_id", item_id = self.item_id(id)),
            self.def_id(item.def_id),
        );
        // type_spec
        if let Some(type_id) = item.type_spec {
            self.emit_edge(
                format!("{item_id}:type_spec", item_id = self.item_id(id)),
                self.type_id(type_id),
            );
        }
        // init_expr
        if let Some(body_id) = item.init_expr {
            self.emit_edge(
                format!("{item_id}:init_expr", item_id = self.item_id(id)),
                self.body_id(body_id),
            );
        }
    }

    fn visit_type_decl(&self, id: item::ItemId, item: &item::Type) {
        self.emit_item(
            id,
            "Type",
            Layout::Vbox(vec![
                Layout::Port("def_id".into()),
                Layout::Port("type_def".into()),
            ]),
        );
        self.emit_def_id(item.def_id);

        let defined_to = match item.type_def {
            item::DefinedType::Alias(alias) => self.type_id(alias),
            item::DefinedType::Forward(span) => {
                let node_id = derived_id(self.item_id(id), "forward");

                self.emit_node(&node_id, "ForwardDef", span, Layout::Empty);
                node_id
            }
        };

        // def_id
        self.emit_edge(
            format!("{item_id}:def_id", item_id = self.item_id(id)),
            self.def_id(item.def_id),
        );
        // type
        self.emit_edge(
            format!("{item_id}:type_def", item_id = self.item_id(id)),
            defined_to,
        );
    }

    fn visit_bind_decl(&self, id: item::ItemId, item: &item::Binding) {
        self.emit_item(
            id,
            "Bind",
            Layout::Vbox(vec![
                Layout::Port("def_id".into()),
                Layout::Hbox(vec![
                    Layout::Node(format!("{muta:?}", muta = item.mutability)),
                    Layout::Node(format!("register: {reg:?}", reg = item.is_register)),
                ]),
                Layout::Port("bind_to".into()),
            ]),
        );
        self.emit_def_id(item.def_id);

        // def_id
        self.emit_edge(
            format!("{item_id}:def_id", item_id = self.item_id(id)),
            self.def_id(item.def_id),
        );
        // bind_to
        self.emit_edge(
            format!("{item_id}:bind_to", item_id = self.item_id(id)),
            self.body_id(item.bind_to),
        );
    }

    fn visit_subprogram_decl(&self, id: item::ItemId, item: &item::Subprogram) {
        let mut v_layout = vec![
            Layout::Node(format!("{kind:?}", kind = item.kind)),
            Layout::Port("def_id".into()),
            Layout::Port("params".into()),
            Layout::Port("result".into()),
            Layout::Port("body".into()),
        ];

        // extra
        match item.extra {
            item::SubprogramExtra::None => {}
            item::SubprogramExtra::DeviceSpec(body_id) => {
                v_layout.push(Layout::Port("dev_spec".into()));
                self.emit_edge(
                    format!("{item_id}:dev_spec", item_id = self.item_id(id)),
                    self.body_id(body_id),
                );
            }
            item::SubprogramExtra::StackSize(body_id) => {
                v_layout.push(Layout::Port("stack_size".into()));
                self.emit_edge(
                    format!("{item_id}:stack_size", item_id = self.item_id(id)),
                    self.body_id(body_id),
                );
            }
        }

        self.emit_item(id, "Subprogram", Layout::Vbox(v_layout));
        self.emit_def_id(item.def_id);

        // def_id
        self.emit_edge(
            format!("{item_id}:def_id", item_id = self.item_id(id)),
            self.def_id(item.def_id),
        );
        // params
        let params_node = {
            let node_id = derived_id(self.item_id(id), "params_list");
            let mut v_layout = vec![];

            if let Some(param_list) = &item.param_list {
                v_layout.push(Layout::Port("params".into()));

                for (name, param) in param_list.names.iter().zip(&param_list.tys) {
                    let def_info = self.library.local_def(*name);
                    let param_node = derived_id(self.item_id(id), &format!("param_{name:?}"));
                    self.emit_node(
                        &param_node,
                        "SubprogParam",
                        def_info.name.span(),
                        Layout::Vbox(vec![
                            self.layout_param_info(param),
                            Layout::Port("def_id".into()),
                            Layout::Port("type".into()),
                        ]),
                    );
                    self.emit_def_id(*name);

                    self.emit_edge(format!("{param_node}:def_id"), self.def_id(*name));
                    self.emit_edge(format!("{param_node}:type"), self.type_id(param.param_ty));

                    // Link to main list
                    self.emit_edge(format!("{node_id}:params"), param_node);
                }
            } else {
                v_layout.push(Layout::Node("".into()));
            }

            self.emit_node(
                &node_id,
                "SubprogramParams",
                self.library.span_map.dummy_span(),
                Layout::Vbox(v_layout),
            );

            node_id
        };
        self.emit_edge(
            format!("{item_id}:params", item_id = self.item_id(id)),
            params_node,
        );

        // result
        let result_node = {
            let node_id = derived_id(self.item_id(id), "result_node");

            let span_at = if let Some(def_id) = item.result.name {
                self.emit_def_id(def_id);
                self.emit_edge(format!("{node_id}:name"), self.def_id(def_id));

                let def_info = self.library.local_def(def_id);
                def_info.name.span()
            } else {
                self.library.span_map.dummy_span()
            };

            self.emit_node(
                &node_id,
                "SubprogramResult",
                span_at,
                Layout::Vbox(vec![
                    Layout::Port("type".into()),
                    Layout::Port("name".into()),
                ]),
            );

            self.emit_edge(format!("{node_id}:type"), self.type_id(item.result.ty));
            node_id
        };
        self.emit_edge(
            format!("{item_id}:result", item_id = self.item_id(id)),
            result_node,
        );

        // body
        self.emit_edge(
            format!("{item_id}:body", item_id = self.item_id(id)),
            self.body_id(item.body.body),
        );
    }

    fn visit_module(&self, id: item::ItemId, item: &item::Module) {
        let name = if item.as_monitor { "Monitor" } else { "Module" };
        self.emit_item(
            id,
            name,
            Layout::Vbox(vec![
                Layout::Port("def_id".into()),
                Layout::Port("exports".into()),
                Layout::Port("body".into()),
            ]),
        );
        self.emit_def_id(item.def_id);

        // def_id
        self.emit_edge(
            format!("{item_id}:def_id", item_id = self.item_id(id)),
            self.def_id(item.def_id),
        );

        // exports
        let export_table = {
            //
            let export_table = derived_id(self.item_id(id), "export_table");
            let mut v_layout = vec![];

            for (idx, export) in item.exports.iter().enumerate() {
                let mut h_layout = vec![
                    Layout::Node(format!("{idx}")),
                    Layout::Node(format!("{muta:?}", muta = export.mutability)),
                    Layout::Node(format!("{qualify:?}", qualify = export.qualify_as)),
                ];
                h_layout.extend(export.is_opaque.then(|| Layout::Node("Opaque".into())));
                h_layout.push(Layout::Vbox(vec![
                    Layout::NamedPort(format!("ex_{idx}_def_id"), "def_id".into()),
                    Layout::NamedPort(format!("ex_{idx}_item_id"), "item_id".into()),
                ]));
                self.emit_def_id(export.def_id);

                self.emit_edge(
                    format!("{export_table}:ex_{idx}_def_id"),
                    self.def_id(export.def_id),
                );
                self.emit_edge(
                    format!("{export_table}:ex_{idx}_item_id"),
                    self.item_id(export.item_id),
                );

                v_layout.push(Layout::Hbox(h_layout));
            }

            self.emit_node(
                &export_table,
                "ExportTable",
                self.library.span_map.dummy_span(),
                Layout::Vbox(v_layout),
            );

            export_table
        };
        self.emit_edge(
            format!("{item_id}:exports", item_id = self.item_id(id)),
            export_table,
        );

        // body
        self.emit_edge(
            format!("{item_id}:body", item_id = self.item_id(id)),
            self.body_id(item.body),
        );
    }

    // Body //
    fn visit_body(&self, id: body::BodyId, body: &body::Body) {
        let body_id = self.body_id(id);
        let (name, layout) = match &body.kind {
            body::BodyKind::Stmts(stmts, in_params, out_ret) => {
                // Connect to in params
                for def_id in in_params {
                    self.emit_edge(self.def_id(*def_id), format!("{body_id}:in_params"));
                }

                // Connect to out return
                if let Some(def_id) = out_ret {
                    self.emit_edge(self.def_id(*def_id), format!("{body_id}:out_ret"));
                }

                // Connect to root stmts
                self.emit_linked_stmts(&body_id, "stmts", id, stmts);

                (
                    "StmtBody",
                    Layout::Vbox(vec![
                        Layout::Port("in_params".into()),
                        Layout::Port("out_ret".into()),
                        Layout::Port("stmts".into()),
                    ]),
                )
            }
            body::BodyKind::Exprs(root) => {
                self.emit_edge(format!("{body_id}:expr"), self.expr_id(BodyExpr(id, *root)));
                ("ExprBody", Layout::Port("expr".into()))
            }
        };

        self.emit_body(id, name, layout);
    }

    // Stmts //
    fn visit_item_stmt(&self, _id: BodyStmt, _item: item::ItemId) {
        // ... poked through
    }

    fn visit_assign(&self, id: BodyStmt, stmt: &stmt::Assign) {
        self.emit_stmt(
            id,
            "Assign",
            Layout::Hbox(vec![Layout::Port("lhs".into()), Layout::Port("rhs".into())]),
        );

        let stmt_id = self.stmt_id(id);
        self.emit_edge(
            format!("{stmt_id}:rhs"),
            self.expr_id(BodyExpr(id.0, stmt.rhs)),
        );
        self.emit_edge(
            format!("{stmt_id}:lhs"),
            self.expr_id(BodyExpr(id.0, stmt.lhs)),
        );
    }

    fn visit_put(&self, id: BodyStmt, stmt: &stmt::Put) {
        let stmt_id = self.stmt_id(id);
        let mut v_layout = vec![];

        v_layout.extend(
            stmt.append_newline
                .then(|| Layout::Node("AppendNewline".into())),
        );

        if let Some(stream_num) = stmt.stream_num {
            v_layout.push(Layout::Port("stream_num".into()));
            self.emit_edge(
                format!("{stmt_id}:stream_num"),
                self.expr_id(BodyExpr(id.0, stream_num)),
            );
        }

        for (idx, item) in stmt.items.iter().enumerate() {
            let item_id = format!("item{idx}");
            match item {
                stmt::Skippable::Skip => v_layout.push(Layout::Hbox(vec![
                    Layout::Node(format!("{idx}:")),
                    Layout::Node("Skip".into()),
                ])),
                stmt::Skippable::Item(item) => {
                    let mut opts_layout =
                        vec![Layout::NamedPort(format!("{item_id}_item"), "item".into())];
                    self.emit_edge(
                        format!("{stmt_id}:{item_id}_item"),
                        self.expr_id(BodyExpr(id.0, item.expr)),
                    );

                    if let Some(expr) = item.opts.width() {
                        let opt_id = format!("{item_id}_width");
                        self.emit_edge(
                            format!("{stmt_id}:{opt_id}"),
                            self.expr_id(BodyExpr(id.0, expr)),
                        );
                        opts_layout.push(Layout::Port(opt_id));
                    }

                    if let Some(expr) = item.opts.precision() {
                        let opt_id = format!("{item_id}_fract");
                        self.emit_edge(
                            format!("{stmt_id}:{opt_id}"),
                            self.expr_id(BodyExpr(id.0, expr)),
                        );
                        opts_layout.push(Layout::Port(opt_id));
                    }

                    if let Some(expr) = item.opts.exponent_width() {
                        let opt_id = format!("{item_id}_exp_width");
                        self.emit_edge(
                            format!("{stmt_id}:{opt_id}"),
                            self.expr_id(BodyExpr(id.0, expr)),
                        );
                        opts_layout.push(Layout::Port(opt_id));
                    }

                    v_layout.push(Layout::Hbox(vec![
                        Layout::Node(format!("{idx}:")),
                        Layout::Vbox(opts_layout),
                    ]));
                }
            }
        }

        self.emit_stmt(id, "Put", Layout::Vbox(v_layout));
    }

    fn visit_get(&self, id: BodyStmt, stmt: &stmt::Get) {
        let stmt_id = self.stmt_id(id);
        let mut v_layout = vec![];

        if let Some(stream_num) = stmt.stream_num {
            v_layout.push(Layout::Port("stream_num".into()));
            self.emit_edge(
                format!("{stmt_id}:stream_num"),
                self.expr_id(BodyExpr(id.0, stream_num)),
            );
        }

        for (idx, item) in stmt.items.iter().enumerate() {
            let item_id = format!("item{idx}");
            match item {
                stmt::Skippable::Skip => v_layout.push(Layout::Hbox(vec![
                    Layout::Node(format!("{idx}:")),
                    Layout::Node("Skip".into()),
                ])),
                stmt::Skippable::Item(item) => {
                    // (already added to layout)
                    self.emit_edge(
                        format!("{stmt_id}:{item_id}_item"),
                        self.expr_id(BodyExpr(id.0, item.expr)),
                    );

                    let width_layout = match item.width {
                        stmt::GetWidth::Token => Layout::Node("Token".into()),
                        stmt::GetWidth::Line => Layout::Node("Line".into()),
                        stmt::GetWidth::Chars(expr) => {
                            let opt_id = format!("{item_id}_chars");
                            self.emit_edge(
                                format!("{stmt_id}:{opt_id}"),
                                self.expr_id(BodyExpr(id.0, expr)),
                            );
                            Layout::NamedPort(opt_id, "Chars".into())
                        }
                    };

                    v_layout.push(Layout::Hbox(vec![
                        Layout::Node(format!("{idx}:")),
                        Layout::Vbox(vec![
                            Layout::NamedPort(format!("{item_id}_item"), "item".into()),
                            width_layout,
                        ]),
                    ]));
                }
            }
        }

        self.emit_stmt(id, "Get", Layout::Vbox(v_layout));
    }

    fn visit_for(&self, id: BodyStmt, stmt: &stmt::For) {
        let stmt_id = self.stmt_id(id);
        let mut v_layout = vec![];

        v_layout.extend(
            stmt.is_decreasing
                .then(|| Layout::Node("Decreasing".into())),
        );

        if let Some(counter_def) = stmt.counter_def {
            v_layout.push(Layout::Port("counter".into()));
            self.emit_def_id(counter_def);
            self.emit_edge(format!("{stmt_id}:counter"), self.def_id(counter_def));
        }

        match stmt.bounds {
            stmt::ForBounds::Implicit(bounds) => {
                v_layout.push(Layout::Port("bounds".into()));
                self.emit_edge(
                    format!("{stmt_id}:bounds"),
                    self.expr_id(BodyExpr(id.0, bounds)),
                );
            }
            stmt::ForBounds::Full(start, end) => {
                v_layout.push(Layout::Hbox(vec![
                    Layout::Port("start".into()),
                    Layout::Port("end".into()),
                ]));
                self.emit_edge(format!("{stmt_id}:end"), self.expr_id(BodyExpr(id.0, end)));
                self.emit_edge(
                    format!("{stmt_id}:start"),
                    self.expr_id(BodyExpr(id.0, start)),
                );
            }
        }

        if let Some(step_by) = stmt.step_by {
            v_layout.push(Layout::Port("step_by".into()));
            self.emit_edge(
                format!("{stmt_id}:step_by"),
                self.expr_id(BodyExpr(id.0, step_by)),
            );
        }

        v_layout.push(Layout::Port("stmts".into()));
        self.emit_linked_stmts(&stmt_id, "stmts", id.0, &stmt.stmts);

        self.emit_stmt(id, "For", Layout::Vbox(v_layout));
    }

    fn visit_loop(&self, id: BodyStmt, stmt: &stmt::Loop) {
        self.emit_stmt(id, "Loop", Layout::Port("stmts".into()));
        self.emit_linked_stmts(&self.stmt_id(id), "stmts", id.0, &stmt.stmts)
    }

    fn visit_exit(&self, id: BodyStmt, stmt: &stmt::Exit) {
        let layout = if let Some(condition) = stmt.when_condition {
            self.emit_edge(
                format!("{stmt_id}:condition", stmt_id = self.stmt_id(id)),
                self.expr_id(BodyExpr(id.0, condition)),
            );
            Layout::Port("condition".into())
        } else {
            Layout::Empty
        };
        self.emit_stmt(id, "Exit", layout);
    }

    fn visit_if(&self, id: BodyStmt, stmt: &stmt::If) {
        self.emit_stmt(
            id,
            "If",
            Layout::Vbox(vec![
                Layout::Port("condition".into()),
                Layout::Port("true_branch".into()),
                Layout::Port("false_branch".into()),
            ]),
        );

        let stmt_id = self.stmt_id(id);
        self.emit_edge(
            format!("{stmt_id}:condition"),
            self.expr_id(BodyExpr(id.0, stmt.condition)),
        );
        self.emit_edge(
            format!("{stmt_id}:true_branch"),
            self.stmt_id(BodyStmt(id.0, stmt.true_branch)),
        );
        if let Some(false_branch) = stmt.false_branch {
            self.emit_edge(
                format!("{stmt_id}:false_branch"),
                self.stmt_id(BodyStmt(id.0, false_branch)),
            );
        }
    }

    fn visit_case(&self, id: BodyStmt, stmt: &stmt::Case) {
        let stmt_id = self.stmt_id(id);
        let mut v_layout = vec![Layout::Port("discrim".into()), Layout::Node("arms".into())];
        self.emit_edge(
            format!("{stmt_id}:discrim"),
            self.expr_id(BodyExpr(id.0, stmt.discriminant)),
        );

        for (idx, arm) in stmt.arms.iter().enumerate() {
            let arm_id = format!("arm{idx}");
            let mut arm_layout = vec![];

            match &arm.selectors {
                stmt::CaseSelector::Default => arm_layout.push(Layout::Node("Default".into())),
                stmt::CaseSelector::Exprs(exprs) => {
                    for (idx, expr) in exprs.iter().enumerate() {
                        let sel_id = format!("{arm_id}_sel{idx}");

                        self.emit_edge(
                            format!("{stmt_id}:{sel_id}"),
                            self.expr_id(BodyExpr(id.0, *expr)),
                        );
                        arm_layout.push(Layout::NamedPort(sel_id, format!("select_{idx}")));
                    }
                }
            }

            let arm_stmts = format!("{arm_id}_stmts");
            self.emit_linked_stmts(&stmt_id, &arm_stmts, id.0, &arm.stmts);
            arm_layout.push(Layout::NamedPort(arm_stmts, "stmts".into()));

            v_layout.push(Layout::Hbox(vec![
                Layout::Node(format!("{idx}:")),
                Layout::Vbox(arm_layout),
            ]));
        }

        self.emit_stmt(id, "Case", Layout::Vbox(v_layout));
    }

    fn visit_block(&self, id: BodyStmt, stmt: &stmt::Block) {
        self.emit_stmt(id, "Block", Layout::Port("stmts".into()));
        self.emit_linked_stmts(&self.stmt_id(id), "stmts", id.0, &stmt.stmts)
    }

    fn visit_call_stmt(&self, id: BodyStmt, stmt: &stmt::Call) {
        let stmt_id = self.stmt_id(id);
        let mut v_layout = vec![Layout::NamedPort("lhs".into(), "".into())];

        if let Some(arguments) = &stmt.arguments {
            v_layout.push(Layout::Node("params".into()));

            if arguments.is_empty() {
                v_layout.push(Layout::Hbox(vec![
                    Layout::Node("".into()),
                    Layout::Node("".into()),
                ]));
            } else {
                for (idx, arg) in arguments.iter().enumerate() {
                    v_layout.push(Layout::Hbox(vec![
                        Layout::Node(format!("{idx}:")),
                        Layout::NamedPort(format!("{idx}"), "".into()),
                    ]));

                    self.emit_edge(
                        format!("{stmt_id}:{idx}"),
                        self.expr_id(BodyExpr(id.0, *arg)),
                    );
                }
            }
        }

        self.emit_stmt(id, "CallStmt", Layout::Vbox(v_layout));

        self.emit_edge(
            format!("{stmt_id}:lhs"),
            self.expr_id(BodyExpr(id.0, stmt.lhs)),
        );
    }

    fn visit_return_stmt(&self, id: BodyStmt, _stmt: &stmt::Return) {
        self.emit_stmt(id, "Return", Layout::Empty);
    }

    fn visit_result_stmt(&self, id: BodyStmt, stmt: &stmt::Result) {
        self.emit_stmt(id, "Result", Layout::Port("expr".into()));
        self.emit_edge(
            format!("{stmt_id}:expr", stmt_id = self.stmt_id(id)),
            self.expr_id(BodyExpr(id.0, stmt.expr)),
        );
    }

    // Exprs //
    fn visit_missing_expr(&self, id: BodyExpr) {
        self.emit_expr(id, "MissingExpr", Layout::Empty);
    }

    fn visit_literal(&self, id: BodyExpr, expr: &expr::Literal) {
        let contents = format!("{expr:?}");
        self.emit_expr(
            id,
            "Literal",
            Layout::Node(format!("{}", contents.escape_debug())),
        );
    }

    fn visit_binary(&self, id: BodyExpr, expr: &expr::Binary) {
        self.emit_expr(
            id,
            "Binary",
            Layout::Vbox(vec![
                Layout::Node(format!("{op:?}", op = expr.op.item())),
                Layout::Hbox(vec![Layout::Port("lhs".into()), Layout::Port("rhs".into())]),
            ]),
        );

        let expr_id = self.expr_id(id);
        self.emit_edge(
            format!("{expr_id}:rhs"),
            self.expr_id(BodyExpr(id.0, expr.rhs)),
        );
        self.emit_edge(
            format!("{expr_id}:lhs"),
            self.expr_id(BodyExpr(id.0, expr.lhs)),
        );
    }

    fn visit_unary(&self, id: BodyExpr, expr: &expr::Unary) {
        self.emit_expr(
            id,
            "Unary",
            Layout::Vbox(vec![
                Layout::Node(format!("{op:?}", op = expr.op.item())),
                Layout::Port("rhs".into()),
            ]),
        );

        let expr_id = self.expr_id(id);
        self.emit_edge(
            format!("{expr_id}:rhs"),
            self.expr_id(BodyExpr(id.0, expr.rhs)),
        );
    }

    fn visit_all_expr(&self, id: BodyExpr) {
        self.emit_expr(id, "All", Layout::Empty);
    }

    fn visit_range_expr(&self, id: BodyExpr, expr: &expr::Range) {
        let expr_id = self.expr_id(id);
        let format_range = |bound: expr::RangeBound| match bound {
            expr::RangeBound::FromStart(_) => "FromStart".to_string(),
            expr::RangeBound::FromEnd(_) => "FromEnd".to_string(),
            expr::RangeBound::AtEnd(_) => "AtEnd".to_string(),
        };

        let inner_expr = |bound: expr::RangeBound| match bound {
            expr::RangeBound::FromStart(expr) | expr::RangeBound::FromEnd(expr) => Some(expr),
            expr::RangeBound::AtEnd(_) => None,
        };

        let layout = if let Some(end) = expr.end {
            if let Some(expr) = inner_expr(end) {
                self.emit_edge(format!("{expr_id}:rhs"), self.expr_id(BodyExpr(id.0, expr)));
            }
            if let Some(expr) = inner_expr(expr.start) {
                self.emit_edge(format!("{expr_id}:lhs"), self.expr_id(BodyExpr(id.0, expr)));
            }

            Layout::Hbox(vec![
                Layout::NamedPort("lhs".into(), format_range(expr.start)),
                Layout::NamedPort("rhs".into(), format_range(end)),
            ])
        } else {
            if let Some(expr) = inner_expr(expr.start) {
                self.emit_edge(format!("{expr_id}:lhs"), self.expr_id(BodyExpr(id.0, expr)));
            }
            Layout::NamedPort("lhs".into(), format_range(expr.start))
        };

        self.emit_expr(id, "Range", layout);
    }

    fn visit_name(&self, id: BodyExpr, expr: &expr::Name) {
        let (name, layout) = match expr {
            expr::Name::Name(def_id) => {
                self.emit_edge(
                    format!("{expr_id}:def_id", expr_id = self.expr_id(id)),
                    self.def_id(*def_id),
                );
                ("Name", Layout::Port("def_id".into()))
            }
            expr::Name::Self_ => ("Self", Layout::Empty),
        };

        self.emit_expr(id, name, layout);
    }

    fn visit_field(&self, id: BodyExpr, expr: &expr::Field) {
        self.emit_expr(
            id,
            "Field",
            Layout::Vbox(vec![
                Layout::Port("lhs".into()),
                Layout::Node(format!("field: '{field}'", field = expr.field.item())),
            ]),
        );

        let expr_id = self.expr_id(id);
        self.emit_edge(
            format!("{expr_id}:lhs"),
            self.expr_id(BodyExpr(id.0, expr.lhs)),
        );
    }

    fn visit_call_expr(&self, id: BodyExpr, expr: &expr::Call) {
        let expr_id = self.expr_id(id);
        let mut v_layout = vec![
            Layout::NamedPort("lhs".into(), "".into()),
            Layout::Node("params".into()),
        ];

        if expr.arguments.is_empty() {
            v_layout.push(Layout::Hbox(vec![
                Layout::Node("".into()),
                Layout::Node("".into()),
            ]));
        } else {
            for (idx, arg) in expr.arguments.iter().enumerate() {
                v_layout.push(Layout::Hbox(vec![
                    Layout::Node(format!("{idx}:")),
                    Layout::NamedPort(format!("{idx}"), "".into()),
                ]));

                self.emit_edge(
                    format!("{expr_id}:{idx}"),
                    self.expr_id(BodyExpr(id.0, *arg)),
                );
            }
        }

        self.emit_expr(id, "CallExpr", Layout::Vbox(v_layout));

        self.emit_edge(
            format!("{expr_id}:lhs"),
            self.expr_id(BodyExpr(id.0, expr.lhs)),
        );
    }

    // Types //
    fn visit_missing_type(&self, id: ty::TypeId) {
        self.emit_type(id, "MissingType", Layout::Empty);
    }

    fn visit_primitive(&self, id: ty::TypeId, ty: &ty::Primitive) {
        let layout = match ty {
            ty::Primitive::SizedChar(ty::SeqLength::Any) => Layout::Node("CharN(Any)".into()),
            ty::Primitive::SizedString(ty::SeqLength::Any) => Layout::Node("StringN(Any)".into()),
            ty::Primitive::SizedChar(ty::SeqLength::Expr(body_id)) => {
                self.emit_edge(
                    format!("{type_id}:body", type_id = self.type_id(id)),
                    self.body_id(*body_id),
                );
                Layout::NamedPort("body".into(), "CharN(Expr)".into())
            }
            ty::Primitive::SizedString(ty::SeqLength::Expr(body_id)) => {
                self.emit_edge(
                    format!("{type_id}:body", type_id = self.type_id(id)),
                    self.body_id(*body_id),
                );
                Layout::NamedPort("body".into(), "StringN(Expr)".into())
            }
            _ => Layout::Node(format!("{ty:?}")),
        };

        self.emit_type(id, "Primitive", layout);
    }

    fn visit_alias(&self, id: ty::TypeId, ty: &ty::Alias) {
        self.emit_type(id, "Alias", Layout::Port("def_id".into()));
        let type_id = self.type_id(id);
        self.emit_edge(format!("{type_id}:def_id"), self.def_id(ty.0));
    }

    fn visit_set(&self, id: ty::TypeId, ty: &ty::Set) {
        self.emit_type(
            id,
            "Set",
            Layout::Vbox(vec![
                Layout::Port("def_id".into()),
                Layout::Port("element".into()),
            ]),
        );
        self.emit_def_id(ty.def_id);

        let type_id = self.type_id(id);
        self.emit_edge(format!("{type_id}:def_id"), self.def_id(ty.def_id));
        self.emit_edge(format!("{type_id}:element"), self.type_id(ty.elem_ty));
    }

    fn visit_subprogram_ty(&self, id: ty::TypeId, ty: &ty::Subprogram) {
        let name = match ty.kind {
            SubprogramKind::Procedure => "ProcedureType",
            SubprogramKind::Function => "FunctionType",
            SubprogramKind::Process => "ProcessType",
        };

        let type_id = self.type_id(id);

        let mut v_layout = vec![Layout::Node("params".into())];
        if let Some(param_list) = &ty.param_list {
            // With params

            for (idx, param) in param_list.iter().enumerate() {
                v_layout.push(Layout::Hbox(vec![
                    Layout::Node(format!("{idx}: ")),
                    self.layout_param_info(param),
                    Layout::NamedPort(format!("params_{idx}_type"), "".into()),
                ]));

                self.emit_edge(
                    format!("{type_id}:params_{idx}_type"),
                    self.type_id(param.param_ty),
                );
            }
        } else {
            // No params
            v_layout.push(Layout::Hbox(vec![
                Layout::Node("".into()),
                Layout::Node("".into()),
            ]));
        }

        v_layout.push(Layout::Port("result".into()));
        self.emit_edge(format!("{type_id}:result"), self.type_id(ty.result_ty));

        self.emit_type(id, name, Layout::Vbox(v_layout));
    }

    fn visit_void(&self, id: ty::TypeId) {
        self.emit_type(id, "Void", Layout::Empty)
    }
}

fn derived_id(mut from_id: String, derived: &str) -> String {
    from_id.pop();
    from_id.push(':');
    from_id.push_str(derived);
    from_id.push('"');
    from_id
}
