//! Pretty-printing HIR trees, as a GraphViz dot file

use std::{
    cell::{Cell, RefCell},
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

pub fn pretty_print_graph(
    library_graph: LibraryGraph,
    get_library: impl Fn(LibraryId) -> LoweredLibrary,
) -> String {
    let mut output = String::new();
    writeln!(output, "digraph hir_graph {{").unwrap();
    writeln!(output, "node [shape=Mrecord]").unwrap();
    writeln!(output, "rankdir=LR").unwrap();

    // Define the contents of the libraries
    for (_, library_id) in library_graph.library_roots() {
        writeln!(output, r#"subgraph "{library_id:?}" {{"#).unwrap();
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
    Hbox(Vec<Layout>),
    Vbox(Vec<Layout>),
    Node(String),
    Port(String),
}

impl Layout {
    fn try_emit_layout(&self, out: &mut dyn fmt::Write) -> fmt::Result {
        match self {
            Layout::Hbox(layouts) => {
                let mut iter = layouts.iter();
                write!(out, "{{ ")?;
                if let Some(layout) = iter.next() {
                    layout.try_emit_layout(out)?;
                }
                for layout in iter {
                    write!(out, " | ")?;
                    layout.try_emit_layout(out)?;
                }
                write!(out, " }}")?;
            }
            Layout::Vbox(layouts) => {
                let mut iter = layouts.iter();
                if let Some(layout) = iter.next() {
                    layout.try_emit_layout(out)?;
                }
                for layout in iter {
                    write!(out, " | ")?;
                    layout.try_emit_layout(out)?;
                }
            }
            Layout::Node(s) => write!(out, "{s}")?,
            Layout::Port(s) => write!(out, "<{s}> {s}")?,
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

        // Graph format is:
        // $id [label="$node_name@($file_id, $span) | $layout"]
        write!(
            out,
            r#"{id} [label="{name}@{span} | "#,
            span = self.display_span(span)
        )?;
        layout.try_emit_layout(out.deref_mut())?;
        writeln!(out, r#" | "]"#)?;

        Ok(())
    }

    fn emit_edge(&self, from: impl Into<String>, to: impl Into<String>) {
        let mut out = self.out.borrow_mut();
        let from = from.into();
        let to = to.into();
        writeln!(&mut *out, "{from} -> {to}").expect("failed to emit edge");
    }

    fn display_span(&self, span: SpanId) -> String {
        let span = self.library.span_map.lookup_span(span);

        if let Some((file_id, range)) = span.into_parts() {
            format!("({file_id:?}, {range:?})")
        } else {
            "(dummy)".to_string()
        }
    }

    fn display_def(&self, def_id: LocalDefId) -> String {
        let name = &self.library.local_def(def_id).name;
        let def_span = self.display_span(name.span());
        format!("{:?}@{}", name.item(), def_span)
    }

    fn display_extra_def(&self, def_id: LocalDefId) -> String {
        let def_info = self.library.local_def(def_id);
        let def_display = self.display_def(def_id);
        match def_info.kind {
            toc_hir::symbol::SymbolKind::Undeclared => {
                format!("{def_display}, undeclared")
            }
            toc_hir::symbol::SymbolKind::Forward(kind, None) => {
                format!("{def_display}, unresolved forward({kind:?})")
            }
            toc_hir::symbol::SymbolKind::Forward(kind, Some(resolve_to)) => {
                format!(
                    "{}, forward({:?}) -> {}",
                    def_display,
                    kind,
                    self.display_def(resolve_to)
                )
            }
            toc_hir::symbol::SymbolKind::Resolved(kind) => {
                format!("{def_display}, resolved({kind:?})")
            }
            _ => def_display,
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

    fn def_of(&self, item: item::ItemId) -> LocalDefId {
        self.library.item(item).def_id
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
}

impl<'out, 'hir> HirVisitor for PrettyVisitor<'out, 'hir> {
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

                self.emit_node(&node_id, "ForwardDef", span, Layout::Vbox(vec![]));
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
                    Layout::Port(format!("{idx}_def_id")),
                    Layout::Port(format!("{idx}_item_id")),
                ]));
                self.emit_def_id(export.def_id);

                self.emit_edge(
                    format!("{export_table}:{idx}_def_id"),
                    self.def_id(export.def_id),
                );
                self.emit_edge(
                    format!("{export_table}:{idx}_item_id"),
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
            body::BodyKind::Stmts(roots, in_params, out_ret) => {
                // Connect to in params
                for def_id in in_params {
                    self.emit_edge(self.def_id(*def_id), format!("{body_id}:in_params"));
                }

                // Connect to out return
                if let Some(def_id) = out_ret {
                    self.emit_edge(self.def_id(*def_id), format!("{body_id}:out_ret"));
                }

                // Connect to root stmts
                for root in roots {
                    // short-circuit directly to the item
                    let to = if let stmt::StmtKind::Item(item_id) = &body.stmt(*root).kind {
                        self.item_id(*item_id)
                    } else {
                        self.stmt_id(BodyStmt(id, *root))
                    };
                    self.emit_edge(format!("{body_id}:stmts"), to);
                }

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
}

fn derived_id(mut from_id: String, derived: &str) -> String {
    from_id.pop();
    from_id.push(':');
    from_id.push_str(derived);
    from_id.push('"');
    from_id
}
