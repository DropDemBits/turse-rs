//! Lowering ungrammar files
use std::collections::HashMap;

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

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub(super) struct NodeId(pub usize);

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub(super) struct TokenId(pub usize);

#[derive(Debug)]
pub(super) struct LoweredGrammar<'s> {
    pub(super) node_data: Vec<Node<'s>>,
    pub(super) token_data: Vec<String>,
}

#[derive(Debug)]
pub(super) struct Node<'s> {
    pub(super) name: &'s str,
    pub(super) children: Vec<NodeChild<'s>>,
}

#[derive(Debug)]
pub(super) struct NodeChild<'s> {
    pub(super) provided_name: Option<&'s str>,
    pub(super) to: ChildKind,
}

#[derive(Debug)]
pub(super) enum ChildKind {
    Item(NodeOrToken),
    List(NodeOrToken),
    Alternate(NodeOrToken),
}

#[derive(Debug)]
pub(super) enum NodeOrToken {
    Node(NodeId),
    Token(TokenId),
}

pub(super) fn lower_grammar(grammar: &Grammar) -> LoweredGrammar {
    let mut ctx = LowerCtx::new();

    // first pass, adding nodes
    for root_node in grammar.iter() {
        ctx.add_node(&grammar[root_node].name);
    }

    for root_node in grammar.iter() {
        visit_node(root_node, grammar, &mut ctx);
    }

    ctx.finish()
}

struct LowerCtx<'s> {
    node_data: Vec<Node<'s>>,
    token_data: Vec<String>,
    name_to_node: HashMap<String, NodeId>,
    name_to_token: HashMap<String, TokenId>,
    next_node_id: usize,
    next_token_id: usize,
}

impl<'s> LowerCtx<'s> {
    fn new() -> Self {
        Self {
            node_data: vec![],
            token_data: vec![],
            name_to_node: HashMap::new(),
            name_to_token: HashMap::new(),
            next_node_id: 0,
            next_token_id: 0,
        }
    }

    fn add_node(&mut self, name: &'s str) -> NodeId {
        let id = NodeId(self.next_node_id);
        self.next_node_id += 1;

        self.name_to_node.insert(name.to_string(), id);
        self.node_data.push(Node {
            name,
            children: vec![],
        });

        id
    }

    fn add_child_to_node(&mut self, node: NodeId, child: NodeChild<'s>) {
        self.node_data[node.0].children.push(child);
    }

    fn add_token(&mut self, name: &str) -> TokenId {
        let id = TokenId(self.next_token_id);
        self.next_token_id += 1;

        self.token_data.push(name.to_string());
        self.name_to_token.insert(name.to_string(), id);

        id
    }

    fn get_token(&mut self, name: &str) -> TokenId {
        if let Some(id) = self.name_to_token.get(name) {
            *id
        } else {
            self.add_token(name)
        }
    }

    fn get_node(&mut self, name: &str) -> Option<NodeId> {
        self.name_to_node.get(name).copied()
    }

    fn finish(self) -> LoweredGrammar<'s> {
        LoweredGrammar {
            node_data: self.node_data,
            token_data: self.token_data,
        }
    }
}

fn visit_node<'g>(node: ungrammar::Node, g: &'g Grammar, ctx: &mut LowerCtx<'g>) {
    let data = &g[node];
    let id = ctx.get_node(&data.name).unwrap();

    visit_rule(&data.rule, id, g, ctx);
}

fn visit_rule<'g>(rule: &'g Rule, parent: NodeId, g: &'g Grammar, ctx: &mut LowerCtx<'g>) {
    if try_as_list(rule, parent, g, ctx).is_some() {
        return;
    }

    match rule {
        Rule::Seq(subs) => {
            // Rule list
            for rule in subs {
                visit_rule(rule, parent, g, ctx);
            }
        }
        Rule::Alt(alts) => {
            // Check complexity
            #[derive(Debug, PartialEq, Clone, Copy)]
            enum Complexity {
                Unknown,
                SimpleNode,
                SimpleToken,
                Complex,
            }

            let mut actual_complexity = Complexity::Unknown;

            for rule in alts {
                fn get_inner_rule_complexity(rule: &Rule) -> Complexity {
                    match rule {
                        Rule::Labeled { rule, .. } => get_inner_rule_complexity(rule),
                        Rule::Node(_) => Complexity::SimpleNode,
                        Rule::Token(_) => Complexity::SimpleToken,
                        _ => Complexity::Complex,
                    }
                }

                let transition_to = get_inner_rule_complexity(rule);

                actual_complexity = match actual_complexity {
                    Complexity::Unknown => transition_to,
                    Complexity::SimpleNode if transition_to == Complexity::SimpleToken => {
                        Complexity::Complex
                    }
                    Complexity::SimpleToken if transition_to == Complexity::SimpleNode => {
                        Complexity::Complex
                    }
                    Complexity::Complex => Complexity::Complex,
                    _ if transition_to == Complexity::Complex => Complexity::Complex,
                    _ => actual_complexity,
                };
            }

            match actual_complexity {
                Complexity::Complex => {
                    // as seq list
                    for rule in alts {
                        visit_rule(rule, parent, g, ctx);
                    }
                }
                Complexity::Unknown => unreachable!(),
                _ => {
                    for rule in alts {
                        let child = visit_terminal_rule(rule, g, ctx);
                        let child = match child.to {
                            ChildKind::Item(to) => NodeChild {
                                provided_name: None,
                                to: ChildKind::Alternate(to),
                            },
                            _ => unreachable!(),
                        };

                        ctx.add_child_to_node(parent, child);
                    }
                }
            }
        }
        Rule::Opt(rule) => {
            // visit inner
            visit_rule(rule, parent, g, ctx);
        }
        _ => {
            let child = visit_terminal_rule(rule, g, ctx);

            ctx.add_child_to_node(parent, child);
        }
    }
}

fn try_as_list<'g>(
    rule: &'g Rule,
    parent: NodeId,
    g: &'g Grammar,
    ctx: &mut LowerCtx<'g>,
) -> Option<()> {
    // ( Item ( ',' Item )* )
    // or name:( Item ( ',' Item )* )

    let (rule, maybe_name) = if let Rule::Labeled {
        label,
        rule: inner_rule,
    } = rule
    {
        (&**inner_rule, Some(label.as_str()))
    } else {
        (rule, None)
    };

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
                            let item = NodeOrToken::Node(ctx.get_node(&g[*item].name).unwrap());

                            let list = NodeChild {
                                provided_name: maybe_name,
                                to: ChildKind::List(item),
                            };

                            ctx.add_child_to_node(parent, list);
                            return Some(());
                        }
                    }
                }
            }
        }
    }

    None
}

fn visit_terminal_rule<'g>(rule: &'g Rule, g: &'g Grammar, ctx: &mut LowerCtx) -> NodeChild<'g> {
    match rule {
        Rule::Labeled { label, rule } => {
            let inner = visit_terminal_rule(rule, g, ctx);

            NodeChild {
                provided_name: Some(label.as_str()),
                ..inner
            }
        }
        Rule::Node(id) => {
            let kind = NodeOrToken::Node(ctx.get_node(&g[*id].name).unwrap());

            NodeChild {
                provided_name: None,
                to: ChildKind::Item(kind),
            }
        }
        Rule::Token(id) => {
            let kind = NodeOrToken::Token(ctx.get_token(&g[*id].name));

            NodeChild {
                provided_name: None,
                to: ChildKind::Item(kind),
            }
        }
        Rule::Rep(inner) => {
            let inner = visit_terminal_rule(inner, g, ctx);

            match inner.to {
                ChildKind::Item(to) => NodeChild {
                    provided_name: None,
                    to: ChildKind::List(to),
                },
                ChildKind::List(_) => inner,
                _ => unreachable!(),
            }
        }
        Rule::Opt(rule) => visit_terminal_rule(rule, g, ctx),
        _ => unreachable!("seq & alt aren't terminal {:?}", rule),
    }
}
