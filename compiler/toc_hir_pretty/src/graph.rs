//! Pretty-printing HIR trees, as a GraphViz dot file

use std::collections::HashSet;
use std::{
    cell::RefCell,
    fmt::{self, Write},
    ops::DerefMut,
};

use toc_hir::library::LibraryId;
use toc_hir::library_graph::SourceGraph;
use toc_hir::symbol::{self, Symbol};
use toc_hir::visitor::WalkNode;
use toc_hir::{
    body,
    expr::{self, BodyExpr},
    item,
    library::{self, LoweredLibrary},
    span::{HasSpanTable, SpanId, Spanned},
    stmt::{self, BodyStmt},
    symbol::{LocalDefId, Mutability, SubprogramKind},
    ty,
    visitor::{HirVisitor, WalkEvent, Walker},
};

const IS_LR_LAYOUT: bool = true;

pub fn pretty_print_graph(
    library_graph: &SourceGraph,
    get_library: impl Fn(LibraryId) -> LoweredLibrary,
) -> String {
    let mut output = String::new();
    writeln!(output, "digraph hir_graph {{").unwrap();
    writeln!(
        output,
        "node [shape=Mrecord style=filled fillcolor=\"white\"]"
    )
    .unwrap();
    writeln!(
        output,
        "rankdir={rank}",
        rank = if IS_LR_LAYOUT { "LR" } else { "BT" }
    )
    .unwrap();
    writeln!(output, "nodesep=0.5").unwrap();

    // Define the contents of the libraries
    for (library_id, _) in library_graph.all_libraries() {
        writeln!(output, r#"subgraph "cluster_{library_id:?}" {{"#).unwrap();
        writeln!(output, r#"label="{library_id:?}""#).unwrap();
        writeln!(output, r##"bgcolor="#009aef22""##).unwrap();
        let library = get_library(library_id);
        let mut walker = Walker::from_library(&library);

        let pretty = PrettyVisitor::new(&mut output, library_id, &library);

        let mut visited_bodies = HashSet::new();
        let mut visited_types = HashSet::new();

        while let Some(event) = walker.peek_event() {
            // Check to see if it's a duplicate body or type
            let skip_tree = match event {
                WalkEvent::Enter(WalkNode::Body(body_id, _)) => !visited_bodies.insert(*body_id),
                WalkEvent::Enter(WalkNode::Type(type_id, _)) => !visited_types.insert(*type_id),
                _ => false,
            };

            if skip_tree {
                walker.skip_event();
                continue;
            }

            let event = walker.next_event().unwrap(); // already have it

            match event {
                WalkEvent::Enter(node) => {
                    if let WalkNode::Body(body_id, _) = &node {
                        let output = &mut *pretty.out.borrow_mut();
                        writeln!(
                            output,
                            r#"subgraph "cluster_{library_id:?}:{body_id:?}" {{"#
                        )
                        .unwrap();
                        writeln!(output, r#"label="{body_id:?}""#).unwrap();
                    }
                }
                WalkEvent::Leave(node) => {
                    node.visit_node(&pretty);

                    if matches!(node, WalkNode::Body(..)) {
                        let output = &mut *pretty.out.borrow_mut();
                        writeln!(output, "}}").unwrap();
                    }
                }
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
        // $id [tooltip="$id at $span" label="$node_name | $layout"]
        write!(
            out,
            r#"{id} [tooltip="{id_esc}@{span}" label="{start}{name} | "#,
            id_esc = id.escape_debug(),
            span = self.display_span(span)
        )?;
        layout.try_emit_layout(out.deref_mut(), None)?;
        writeln!(out, r#"{end}"]"#)?;

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
        let span = span.lookup_in(self.library);

        if let Some((file_id, range)) = span.into_parts() {
            format!("({file_id:?}, {range:?})")
        } else {
            "(dummy)".to_string()
        }
    }

    fn display_def_id(&self, def_id: LocalDefId) -> String {
        let name = escape_def_name(self.library.local_def(def_id).name);

        format!("'{name}'\\n({def_id:?})")
    }

    fn display_binding(&self, binding: Spanned<Symbol>) -> String {
        match self.library.binding_resolve(binding) {
            symbol::Resolve::Def(def_id) => {
                let def_info = self.library.local_def(def_id);
                let name = escape_def_name(def_info.name);

                format!("'{name}'\\n({def_id:?})")
            }
            symbol::Resolve::Err => {
                let name = escape_def_name(*binding.item());

                format!("'{name}'\\n(undeclared)")
            }
        }
    }

    fn display_extra_def_resolve(&self, local_def: symbol::LocalDefId) -> String {
        match self.library.def_resolve(local_def) {
            symbol::DefResolve::Local(local_def) => {
                format!("local({})", self.display_def_id(local_def))
            }
            symbol::DefResolve::External(def) => {
                format!("external({def:?})")
            }
            symbol::DefResolve::Err => "unresolved".to_string(),
            symbol::DefResolve::Canonical => "".to_string(),
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

    fn emit_def_info(&self, def_id: LocalDefId) {
        let def_info = self.library.local_def(def_id);
        let name = escape_def_name(def_info.name);

        self.emit_node(
            &self.def_id(def_id),
            &format!("{def_id:?}"),
            def_info.def_at,
            Layout::Vbox(vec![
                Layout::Node(format!("name: '{name}'")),
                Layout::Node(format!(
                    "at: '{span}'",
                    span = self.display_span(def_info.def_at)
                )),
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
        // Define all of the defs nodes first
        for def_id in library.local_defs() {
            self.emit_def_info(def_id);
        }
    }

    // Items //
    fn visit_constvar(&self, id: item::ItemId, item: &item::ConstVar) {
        let name = match item.mutability {
            Mutability::Const => "Const",
            Mutability::Var => "Var",
        };

        self.emit_item(
            id,
            name,
            Layout::Vbox(vec![
                Layout::Node(self.display_def_id(item.def_id)),
                Layout::Node(if item.is_register {
                    "Register".into()
                } else {
                    "Storage".into()
                }),
                Layout::Port("type".into()),
                Layout::Port("expr".into()),
            ]),
        );

        // type_spec
        if let Some(type_id) = item.type_spec {
            self.emit_edge(
                format!("{item_id}:type", item_id = self.item_id(id)),
                self.type_id(type_id),
            );
        }
        // init_expr
        if let Some(body_id) = item.init_expr {
            self.emit_edge(
                format!("{item_id}:expr", item_id = self.item_id(id)),
                self.body_id(body_id),
            );
        }
    }

    fn visit_type_decl(&self, id: item::ItemId, item: &item::Type) {
        self.emit_item(
            id,
            "Type",
            Layout::Vbox(vec![
                Layout::Node(self.display_def_id(item.def_id)),
                Layout::Port("type_def".into()),
            ]),
        );

        let defined_to = match item.type_def {
            item::DefinedType::Alias(alias) => self.type_id(alias),
            item::DefinedType::Forward(span) => {
                let node_id = derived_id(self.item_id(id), "forward");

                self.emit_node(&node_id, "ForwardDef", span, Layout::Empty);
                node_id
            }
        };

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
                Layout::Node(self.display_def_id(item.def_id)),
                Layout::Hbox(vec![
                    Layout::Node(format!("{muta:?}", muta = item.mutability)),
                    Layout::Node(format!("register: {reg:?}", reg = item.is_register)),
                ]),
                Layout::Port("bind_to".into()),
            ]),
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
            Layout::Node(self.display_def_id(item.def_id)),
            Layout::Port("params".into()),
            Layout::Port("result".into()),
            Layout::Port("imports".into()),
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
                        def_info.def_at,
                        Layout::Vbox(vec![
                            self.layout_param_info(param),
                            Layout::Node(self.display_def_id(*name)),
                            Layout::Port("type".into()),
                        ]),
                    );

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
                self.library.span_table().dummy_span(),
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

            let (span_at, result_name) = if let Some(def_id) = item.result.name {
                let def_info = self.library.local_def(def_id);
                (def_info.def_at, self.display_def_id(def_id))
            } else {
                (self.library.span_table().dummy_span(), "".into())
            };

            self.emit_node(
                &node_id,
                "SubprogramResult",
                span_at,
                Layout::Vbox(vec![Layout::Port("type".into()), Layout::Node(result_name)]),
            );

            self.emit_edge(format!("{node_id}:type"), self.type_id(item.result.ty));
            node_id
        };
        self.emit_edge(
            format!("{item_id}:result", item_id = self.item_id(id)),
            result_node,
        );

        // Just attach imports to this item
        for &import in &item.body.imports {
            self.emit_edge(
                format!("{item_id}:imports", item_id = self.item_id(id)),
                self.item_id(import),
            );
        }

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
                Layout::Node(self.display_def_id(item.def_id)),
                Layout::Port("imports".into()),
                Layout::Port("exports".into()),
                Layout::Port("body".into()),
            ]),
        );

        // Just attach imports to this item
        for &import in &item.imports {
            self.emit_edge(
                format!("{item_id}:imports", item_id = self.item_id(id)),
                self.item_id(import),
            );
        }

        // exports
        let export_table = {
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
                    Layout::NamedPort(
                        format!("ex_{idx}_def_id"),
                        self.display_def_id(export.def_id),
                    ),
                    Layout::NamedPort(format!("ex_{idx}_exported_def"), "exported_def".into()),
                ]));

                self.emit_edge(
                    format!("{export_table}:ex_{idx}_exported_def"),
                    self.def_id(export.def_id),
                );

                v_layout.push(Layout::Hbox(h_layout));
            }

            self.emit_node(
                &export_table,
                "ExportTable",
                self.library.span_table().dummy_span(),
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

    fn visit_import(&self, id: item::ItemId, item: &item::Import) {
        self.emit_item(
            id,
            "Import",
            Layout::Vbox(vec![
                Layout::Node(self.display_def_id(item.def_id)),
                Layout::Node(self.display_extra_def_resolve(item.def_id)),
                Layout::Node(format!("{:?}", item.mutability)),
            ]),
        )
    }

    // Body //
    fn visit_body(&self, id: body::BodyId, body: &body::Body) {
        let body_id = self.body_id(id);
        let (name, layout) = match &body.kind {
            body::BodyKind::Stmts(stmts, in_params, out_ret) => {
                // Connect to in params
                let mut v_layout = vec![];
                for def_id in in_params {
                    v_layout.push(Layout::Node(format!(
                        "[in] {def}",
                        def = self.display_def_id(*def_id)
                    )));
                }

                // Connect to out return
                if let Some(def_id) = out_ret {
                    v_layout.push(Layout::Node(format!(
                        "[out] {def}",
                        def = self.display_def_id(*def_id)
                    )));
                }

                v_layout.push(Layout::Port("stmts".into()));

                // Connect to root stmts
                self.emit_linked_stmts(&body_id, "stmts", id, stmts);

                ("StmtBody", Layout::Vbox(v_layout))
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
            Layout::Hbox(vec![
                Layout::Empty,
                Layout::Vbox(vec![Layout::Port("lhs".into()), Layout::Port("rhs".into())]),
            ]),
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
            v_layout.push(Layout::Node(self.display_def_id(counter_def)));
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
                    Layout::Node("bounds".into()),
                    Layout::Vbox(vec![
                        Layout::Port("start".into()),
                        Layout::Port("end".into()),
                    ]),
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
        if let Some(false_branch) = stmt.false_branch.stmt() {
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
                    arm_layout.push(Layout::Node("selectors".into()));

                    for (idx, expr) in exprs.iter().enumerate() {
                        let sel_id = format!("{arm_id}_sel{idx}");

                        self.emit_edge(
                            format!("{stmt_id}:{sel_id}"),
                            self.expr_id(BodyExpr(id.0, *expr)),
                        );
                        arm_layout.push(Layout::Hbox(vec![
                            Layout::Node(format!("{idx}:")),
                            Layout::NamedPort(sel_id, "".into()),
                        ]));
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
        let mut v_layout = vec![Layout::Port("lhs".into())];

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

    fn visit_init_expr(&self, id: BodyExpr, expr: &expr::Init) {
        let expr_id = self.expr_id(id);
        let mut v_layout = vec![];

        for (idx, &body_id) in expr.exprs.iter().enumerate() {
            v_layout.push(Layout::Hbox(vec![
                Layout::Node(format!("{idx}")),
                Layout::NamedPort(format!("{expr_id}:body_{idx}"), "".into()),
            ]));
            self.emit_edge(format!("{expr_id}:body_{idx}"), self.body_id(body_id))
        }

        self.emit_expr(id, "InitExpr", Layout::Vbox(v_layout));
    }

    fn visit_binary(&self, id: BodyExpr, expr: &expr::Binary) {
        self.emit_expr(
            id,
            "Binary",
            Layout::Vbox(vec![
                Layout::Node(format!("{op:?}", op = expr.op.item())),
                Layout::Hbox(vec![
                    Layout::Empty,
                    Layout::Vbox(vec![Layout::Port("lhs".into()), Layout::Port("rhs".into())]),
                ]),
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
            expr::Name::Name(binding) => ("Name", Layout::Node(self.display_binding(*binding))),
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
                Layout::Node(format!("'{field}'", field = expr.field.item())),
            ]),
        );

        let expr_id = self.expr_id(id);
        self.emit_edge(
            format!("{expr_id}:lhs"),
            self.expr_id(BodyExpr(id.0, expr.lhs)),
        );
    }

    fn visit_deref(&self, id: BodyExpr, expr: &expr::Deref) {
        let expr_id = self.expr_id(id);
        self.emit_expr(id, "Deref", Layout::Port("rhs".into()));

        self.emit_edge(
            format!("{expr_id}:rhs"),
            self.expr_id(BodyExpr(id.0, expr.rhs)),
        );
    }

    fn visit_call_expr(&self, id: BodyExpr, expr: &expr::Call) {
        let expr_id = self.expr_id(id);
        let mut v_layout = vec![Layout::Port("lhs".into()), Layout::Node("params".into())];

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
        let mut name = self.display_binding(ty.base_def);
        let base_name = name.clone();

        for segment in &ty.segments {
            name.push('.');
            name.push_str(segment.item().name());
        }

        self.emit_type(id, "Alias", Layout::Node(base_name));
    }

    fn visit_constrained(&self, id: ty::TypeId, ty: &ty::Constrained) {
        let type_id = self.type_id(id);
        let mut v_layout = vec![Layout::Port("begin".into())];

        self.emit_edge(format!("{type_id}:begin"), self.body_id(ty.start));

        match ty.end {
            ty::ConstrainedEnd::Expr(end) => {
                v_layout.push(Layout::Port("end".into()));
                self.emit_edge(format!("{type_id}:end"), self.body_id(end));
            }
            ty::ConstrainedEnd::Unsized(sz) => {
                v_layout.push(Layout::Node(format!("Unsized({sz:?})", sz = sz.item())))
            }
            ty::ConstrainedEnd::Any(_) => v_layout.push(Layout::Node("Any".into())),
        }

        self.emit_type(id, "Constrained", Layout::Vbox(v_layout));
    }

    fn visit_enum(&self, id: ty::TypeId, ty: &ty::Enum) {
        let mut v_layout = vec![Layout::Node(self.display_def_id(ty.def_id))];
        v_layout.extend(
            ty.variants
                .iter()
                .map(|&variant| Layout::Node(self.display_def_id(variant))),
        );

        self.emit_type(id, "Enum", Layout::Vbox(v_layout))
    }

    fn visit_array(&self, id: ty::TypeId, ty: &ty::Array) {
        let mut v_layout = vec![Layout::Node(format!("{:?}", ty.sizing))];
        v_layout.extend(
            ty.ranges
                .iter()
                .enumerate()
                .map(|(idx, _range)| Layout::Port(format!("dim{idx}"))),
        );
        v_layout.push(Layout::Port("element".into()));

        self.emit_type(id, "Array", Layout::Vbox(v_layout));

        let type_id = self.type_id(id);
        for (idx, range_ty) in ty.ranges.iter().enumerate() {
            self.emit_edge(format!("{type_id}:dim{idx}"), self.type_id(*range_ty));
        }
    }

    fn visit_set(&self, id: ty::TypeId, ty: &ty::Set) {
        self.emit_type(
            id,
            "Set",
            Layout::Vbox(vec![
                Layout::Node(self.display_def_id(ty.def_id)),
                Layout::Port("element".into()),
            ]),
        );

        let type_id = self.type_id(id);
        self.emit_edge(format!("{type_id}:element"), self.type_id(ty.elem_ty));
    }

    fn visit_pointer(&self, id: ty::TypeId, ty: &ty::Pointer) {
        self.emit_type(
            id,
            "Pointer",
            Layout::Vbox(vec![
                Layout::Node(format!("{checked:?}", checked = ty.checked)),
                Layout::Port("type".into()),
            ]),
        );

        let type_id = self.type_id(id);
        self.emit_edge(format!("{type_id}:type"), self.type_id(ty.ty));
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
            v_layout.push(Layout::Hbox(vec![Layout::Empty, Layout::Empty]));
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

fn escape_def_name(name: Symbol) -> String {
    name.name()
        .escape_debug()
        .flat_map(|c| {
            match c {
                '<' => vec!['&', 'l', 't', ';'],
                '>' => vec!['&', 'g', 't', ';'],
                _ => vec![c],
            }
            .into_iter()
        })
        .collect::<String>()
}
