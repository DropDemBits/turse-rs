//! Lowering ungrammar files
use std::collections::BTreeSet;

use ungrammar::{Grammar, Rule};

/*
Structure:
Node
children: [
    Item(Item)
    List(Item)
]
Group
[
    Alternate(Item)
    ...
]
Item {
    provided_name: Option<str>
    to: NodeOrToken
}
*/

#[derive(Debug)]
pub(super) struct LoweredGrammar {
    pub(super) nodes: Vec<Node>,
    pub(super) groups: Vec<Group>,
    pub(super) tokens: Vec<String>,
    group_names: BTreeSet<String>,
}

impl LoweredGrammar {
    pub(super) fn is_group(&self, item_name: &str) -> bool {
        self.group_names.contains(item_name)
    }
}

#[derive(Debug)]
pub(super) struct Group {
    pub(super) name: String,
    pub(super) variants: Vec<Variant>,
}

#[derive(Debug)]
pub(super) struct Variant {
    pub(super) provided_name: Option<String>,
    pub(super) variant: String,
}

#[derive(Debug)]
pub(super) struct Node {
    pub(super) name: String,
    pub(super) children: Vec<Child>,
}

#[derive(Debug)]
pub(super) struct Child {
    pub(super) provided_name: Option<String>,
    pub(super) kind: ChildKind,
}

#[derive(Debug)]
pub(super) enum ChildKind {
    Item(NodeOrToken),
    List(NodeOrToken),
}

impl PartialEq for ChildKind {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Self::Item(a) => {
                if let Self::Item(b) = other {
                    a == b
                } else {
                    false
                }
            }
            Self::List(a) => {
                if let Self::List(b) = other {
                    a == b
                } else {
                    false
                }
            }
        }
    }
}

#[derive(Debug)]
pub(super) enum NodeOrToken {
    Node(String),
    Token(String),
}

impl PartialEq for NodeOrToken {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Self::Token(a) => {
                if let Self::Token(b) = other {
                    a == b
                } else {
                    false
                }
            }
            _ => false,
        }
    }
}

pub(super) fn lower_grammar(grammar: &Grammar) -> LoweredGrammar {
    let mut ctx = LowerCtx::new();

    for root_node in grammar.iter() {
        visit_node(root_node, grammar, &mut ctx);
    }

    ctx.finish()
}

struct LowerCtx {
    node_data: Vec<Node>,
    group_data: Vec<Group>,
    token_data: Vec<String>,
    group_names: BTreeSet<String>,
}

impl LowerCtx {
    fn new() -> Self {
        Self {
            node_data: vec![],
            group_data: vec![],
            token_data: vec![],
            group_names: BTreeSet::new(),
        }
    }

    fn finish(self) -> LoweredGrammar {
        LoweredGrammar {
            nodes: self.node_data,
            groups: self.group_data,
            tokens: self.token_data,
            group_names: self.group_names,
        }
    }
}

fn visit_node(node: ungrammar::Node, g: &Grammar, ctx: &mut LowerCtx) {
    let data = &g[node];

    if let Some(variants) = try_as_group(&data.rule, g) {
        ctx.group_data.push(Group {
            name: data.name.clone(),
            variants,
        });
        ctx.group_names.insert(data.name.clone());
    } else {
        let mut children = vec![];
        visit_rule(&data.rule, g, &mut children);

        let mut i = 0;
        'next: while i < children.len() {
            for j in 0..i {
                if children[i].kind == children[j].kind {
                    children.remove(i);
                    continue 'next;
                }
            }
            i += 1;
        }
        children.dedup_by(|a, b| a.kind == b.kind);

        ctx.node_data.push(Node {
            name: data.name.clone(),
            children,
        })
    }
}

fn visit_rule<'g>(rule: &'g Rule, g: &'g Grammar, children: &mut Vec<Child>) {
    if let Some(child) = try_as_list(rule, g) {
        // push list
        children.push(child);
        return;
    }

    match rule {
        Rule::Alt(rules) | Rule::Seq(rules) => {
            // Rule list
            for rule in rules {
                visit_rule(rule, g, children);
            }
        }
        Rule::Opt(rule) => {
            // visit inner
            visit_rule(rule, g, children);
        }
        _ => {
            if let Some(child) = visit_terminal_rule(rule, g, children) {
                children.push(child);
            }
        }
    }
}

fn try_as_list<'g>(rule: &'g Rule, g: &'g Grammar) -> Option<Child> {
    // ( Item ( ',' Item )* )
    // or name:( Item ( ',' Item )* )

    // try and get child name
    let (rule, maybe_name) = if let Rule::Labeled {
        label,
        rule: inner_rule,
    } = rule
    {
        (&**inner_rule, Some(label.as_str()))
    } else {
        (rule, None)
    };

    // Get rules
    let mut rules = if let Rule::Seq(subs) = rule {
        subs.iter()
    } else {
        return None;
    };

    if let Rule::Node(item) = rules.next()? {
        if let Rule::Rep(inner) = rules.next()? {
            if let Rule::Seq(others) = &**inner {
                let mut inner_rules = others.iter();

                if let Rule::Token(_tk) = inner_rules.next()? {
                    if let Rule::Node(other_item) = inner_rules.next()? {
                        // both iters must be exhausted
                        if rules.next().is_none()
                            && inner_rules.next().is_none()
                            && item == other_item
                        {
                            // It's a list
                            let item = NodeOrToken::Node(g[*item].name.clone());

                            let list = Child {
                                provided_name: maybe_name.map(|s| s.to_string()),
                                kind: ChildKind::List(item),
                            };

                            return Some(list);
                        }
                    }
                }
            }
        }
    }

    None
}

fn try_as_group(rule: &Rule, g: &Grammar) -> Option<Vec<Variant>> {
    let alts = if let Rule::Alt(alts) = rule {
        alts
    } else {
        // not a group
        return None;
    };

    let mut variants = vec![];
    for rule in alts {
        match rule {
            Rule::Labeled { label, rule } => {
                if let Rule::Node(nd) = &**rule {
                    variants.push(Variant {
                        provided_name: Some(label.clone()),
                        variant: g[*nd].name.clone(),
                    });
                } else {
                    return None;
                }
            }
            Rule::Node(nd) => {
                variants.push(Variant {
                    provided_name: None,
                    variant: g[*nd].name.clone(),
                });
            }
            _ => return None,
        }
    }

    // need dedup?

    Some(variants)
}

fn visit_terminal_rule(rule: &Rule, g: &Grammar, children: &mut Vec<Child>) -> Option<Child> {
    match rule {
        Rule::Labeled { label, rule } => {
            let manual_impl = &[
                "lhs",
                "op",
                "rhs",
                "literal",
                "attr",
                "asn_kind",
                "io_kind",
                "tag_ref",
                "tag_val",
                "wait_ref",
                "wait_arg",
                "bit_range",
                "prim",
                "start",
                "end",
                "width",
                "fraction",
                "exp_width",
            ];

            if manual_impl.contains(&label.as_str()) {
                // skip over for now
                return None;
            }

            let inner = visit_terminal_rule(rule, g, children)?;

            Some(Child {
                provided_name: Some(label.clone()),
                ..inner
            })
        }
        Rule::Node(id) => {
            let kind = NodeOrToken::Node(g[*id].name.clone());

            Some(Child {
                provided_name: None,
                kind: ChildKind::Item(kind),
            })
        }
        Rule::Token(id) => {
            let kind = NodeOrToken::Token(g[*id].name.clone());

            Some(Child {
                provided_name: None,
                kind: ChildKind::Item(kind),
            })
        }
        Rule::Rep(rule) => {
            let inner = visit_terminal_rule(rule, g, children)?;

            Some(Child {
                provided_name: None,
                kind: match inner.kind {
                    ChildKind::Item(item) => ChildKind::List(item),
                    ChildKind::List(_) => inner.kind,
                },
            })
        }
        Rule::Opt(rule) => visit_terminal_rule(rule, g, children),
        _ => {
            // chain back up
            visit_rule(rule, g, children);
            None
        }
    }
}
