mod lowering;

use std::path::{Path, PathBuf};
use std::{env, fs};

use anyhow::{Context, Result};
use heck::SnakeCase;
use lowering::LoweredGrammar;
use quote::{format_ident, quote};
use xshell::cmd;

use crate::lowering::ChildKind;

pub fn project_root() -> PathBuf {
    // from rust-analyzer's xtask lib.rs file
    Path::new(
        &env::var("CARGO_MANIFEST_DIR").unwrap_or_else(|_| env!("CARGO_MANIFEST_DIR").to_owned()),
    )
    .ancestors()
    .nth(1)
    .unwrap()
    .to_path_buf()
}

pub fn do_codegen() -> Result<()> {
    let grammar = project_root().join("compiler/toc_syntax/parsed_turing.ungram");
    let out_dir = project_root().join("compiler/toc_syntax/src/");

    let grammar = fs::read_to_string(&grammar)?.parse()?;
    let lowered = lowering::lower_grammar(&grammar);

    let generated_nodes = out_dir.join("ast/nodes.rs");
    let contents = generate_nodes(&lowered)?;

    // check for rustfmt
    cmd!("rustfmt --version")
        .echo_cmd(false)
        .run()
        .context("failed to run rustfmt")?;

    let formatted = cmd!("rustfmt --config fn_single_line=true --config max_width=120")
        .stdin(contents)
        .output()?
        .stdout;

    fs::write(generated_nodes.as_path(), formatted)?;

    Ok(())
}

fn generate_nodes(lowered: &LoweredGrammar) -> Result<String> {
    let lowered_groups = lowered.groups.iter().map(|group| {
        // as an enum
        let name = format_ident!("{}", group.name);

        let (variants, nodes, kinds): (Vec<_>, Vec<_>, Vec<_>) = group
            .variants
            .iter()
            .map(|child| {
                if lowered.is_group(&child.variant) {
                    // match using a guard
                    let name = format_ident!("{}", child.variant);
                    let node = format_ident!("{}", child.variant);
                    let cast_to = name.clone();
                    (name, node, quote! {_ if #cast_to::can_cast(&syntax)})
                } else {
                    let name = if let Some(other_name) = &child.provided_name {
                        other_name
                    } else {
                        &child.variant
                    };

                    let name = format_ident!("{}", name);
                    let node = format_ident!("{}", child.variant);
                    let sykind_name = format_ident!("{}", child.variant);
                    (name, node, quote! {SyntaxKind::#sykind_name})
                }
            })
            .fold((vec![], vec![], vec![]), |mut acc, thing| {
                acc.0.push(thing.0);
                acc.1.push(thing.1);
                acc.2.push(thing.2);
                acc
            });

        quote! {
            #[derive(Debug, PartialEq, Eq, Hash)]
            pub enum #name {
                #(#variants(#nodes)),*
            }
            impl AstNode for #name {
                fn cast(syntax: SyntaxNode) -> Option<Self> {
                    match syntax.kind() {
                        #(#kinds => Some(Self::#variants(AstNode::cast(syntax)?)),)*
                        _ => None
                    }
                }

                fn can_cast(syntax: &SyntaxNode) -> bool {
                    match syntax.kind() {
                        #(#kinds => true,)*
                        _ => false
                    }
                }

                fn syntax(&self) -> &SyntaxNode {
                    match self {
                        #(Self::#variants(node) => &node.syntax()),*
                    }
                }
            }

        }
    });

    let lowered_nodes = lowered.nodes.iter().map(|node| {
        // as a plain old struct
        let name = format_ident!("{}", node.name);
        let kind_variant = format_ident!("{}", node.name);
        // for disambiguating between Reference and Expr
        let mut reference_count: usize = 0;
        let mut expr_count: usize = 0;

        let methods = node.children.iter().map(|child| {
            let (name, body, res) = match &child.kind {
                ChildKind::Item(item) => match item {
                    lowering::NodeOrToken::Node(node) => {
                        let name = format_ident!("{}", thing_to_method_name(node).to_snake_case());
                        let body = match node.as_str() {
                            "Reference" => {
                                let body = quote! { helper::nodes(&self.0).nth(#reference_count) };
                                reference_count += 1;
                                expr_count += 1;
                                body
                            }
                            "Expr" => {
                                let body = quote! { helper::nodes(&self.0).nth(#expr_count) };
                                expr_count += 1;
                                body
                            }
                            _ => quote! { helper::node(&self.0) },
                        };
                        let res = format_ident!("{}", node);
                        let res = quote! { Option<#res> };
                        (name, body, res)
                    }
                    lowering::NodeOrToken::Token(token) => {
                        let name =
                            format_ident!("{}_token", thing_to_method_name(token).to_snake_case());
                        let syname = format_ident!("{}", token_to_syntax_name(&token));
                        let body = quote! { helper::token(&self.0, SyntaxKind::#syname) };
                        let res = quote! { Option<SyntaxToken> };
                        (name, body, res)
                    }
                },
                ChildKind::List(item) => match item {
                    lowering::NodeOrToken::Node(node) => {
                        let name = format_ident!("{}", thing_to_method_name(node).to_snake_case());
                        let body = quote! { helper::nodes(&self.0) };
                        let res = format_ident!("{}", node);
                        let res = quote! { impl Iterator<Item = #res> + '_ };
                        (name, body, res)
                    }
                    lowering::NodeOrToken::Token(_) => {
                        unreachable!("no use for token lists")
                    }
                },
            };

            let name = if let Some(preferred_name) = &child.provided_name {
                format_ident!("{}", preferred_name.to_snake_case())
            } else {
                name
            };

            quote! {
                pub fn #name (&self) -> #res {
                    #body
                }
            }
        });

        quote! {
            #[derive(Debug, PartialEq, Eq, Hash)]
            #[repr(transparent)]
            pub struct #name(SyntaxNode);

            impl AstNode for #name {
                fn cast(syntax: SyntaxNode) -> Option<Self> {
                    match syntax.kind() {
                        SyntaxKind::#kind_variant => Some(Self(syntax)),
                        _ => None,
                    }
                }

                fn can_cast(syntax: &SyntaxNode) -> bool {
                    match syntax.kind() {
                        SyntaxKind::#kind_variant => true,
                        _ => false
                    }
                }

                fn syntax(&self) -> &SyntaxNode {
                    &self.0
                }
            }

            impl #name {
                #(#methods)*
            }

        }
    });

    let everything = quote! {
        use crate::{ast::{AstNode, helper}, SyntaxKind, SyntaxNode, SyntaxToken};

        #(#lowered_nodes)*
        #(#lowered_groups)*
    };

    let everything = format!(
        "// This is a generated file, do not touch\n{}",
        everything.to_string()
    );

    Ok(everything)
}

fn token_to_syntax_name(token: &str) -> &str {
    match token {
        "@" => "At",
        "&" => "Ampersand",
        "->" => "Arrow",
        "^" => "Caret",
        ":" => "Colon",
        ":=" => "Assign",
        "," => "Comma",
        ".." => "Range",
        "." => "Dot",
        "=" => "Equ",
        ">=" => "GreaterEqu",
        ">" => "Greater",
        "#" => "Pound",
        "=>" => "Imply",
        "<=" => "LessEqu",
        "(" => "LeftParen",
        "<" => "Less",
        "-" => "Minus",
        "+" => "Plus",
        "|" => "Pipe",
        ")" => "RightParen",
        ";" => "Semicolon",
        "/" => "Slash",
        "*" => "Star",
        "**" => "Exp",
        "~" => "Tilde",
        "addressint" => "KwAddressint",
        "all" => "KwAll",
        "and" => "KwAnd",
        "array" => "KwArray",
        "asm" => "KwAsm",
        "assert" => "KwAssert",
        "begin" => "KwBegin",
        "bind" => "KwBind",
        "bits" => "KwBits",
        "body" => "KwBody",
        "boolean" => "KwBoolean",
        "break" => "KwBreak",
        "by" => "KwBy",
        "case" => "KwCase",
        "char" => "KwChar",
        "cheat" => "KwCheat",
        "checked" => "KwChecked",
        "class" => "KwClass",
        "close" => "KwClose",
        "collection" => "KwCollection",
        "condition" => "KwCondition",
        "const" => "KwConst",
        "decreasing" => "KwDecreasing",
        "def" => "KwDef",
        "deferred" => "KwDeferred",
        "div" => "KwDiv",
        "elif" => "KwElif",
        "else" => "KwElse",
        "elseif" => "KwElseif",
        "elsif" => "KwElsif",
        "end" => "KwEnd",
        "endcase" => "KwEndCase",
        "endfor" => "KwEndFor",
        "endif" => "KwEndIf",
        "endloop" => "KwEndLoop",
        "enum" => "KwEnum",
        "exit" => "KwExit",
        "export" => "KwExport",
        "external" => "KwExternal",
        "false" => "KwFalse",
        "flexible" => "KwFlexible",
        "for" => "KwFor",
        "fork" => "KwFork",
        "forward" => "KwForward",
        "free" => "KwFree",
        "function" => "KwFunction",
        "get" => "KwGet",
        "handler" => "KwHandler",
        "if" => "KwIf",
        "implement" => "KwImplement",
        "import" => "KwImport",
        "in" => "KwIn",
        "include" => "KwInclude",
        "inherit" => "KwInherit",
        "init" => "KwInit",
        "int" => "KwInt",
        "int1" => "KwInt1",
        "int2" => "KwInt2",
        "int4" => "KwInt4",
        "invariant" => "KwInvariant",
        "label" => "KwLabel",
        "loop" => "KwLoop",
        "mod" => "KwMod",
        "module" => "KwModule",
        "monitor" => "KwMonitor",
        "nat" => "KwNat",
        "nat1" => "KwNat1",
        "nat2" => "KwNat2",
        "nat4" => "KwNat4",
        "new" => "KwNew",
        "nil" => "KwNil",
        "not" => "KwNot",
        "objectclass" => "KwObjectClass",
        "of" => "KwOf",
        "opaque" => "KwOpaque",
        "open" => "KwOpen",
        "or" => "KwOr",
        "packed" => "KwPacked",
        "pause" => "KwPause",
        "pervasive" => "KwPervasive",
        "pointer" => "KwPointer",
        "post" => "KwPost",
        "pre" => "KwPre",
        "priority" => "KwPriority",
        "procedure" => "KwProcedure",
        "process" => "KwProcess",
        "put" => "KwPut",
        "quit" => "KwQuit",
        "read" => "KwRead",
        "real" => "KwReal",
        "real4" => "KwReal4",
        "real8" => "KwReal8",
        "record" => "KwRecord",
        "register" => "KwRegister",
        "rem" => "KwRem",
        "result" => "KwResult",
        "return" => "KwReturn",
        "seek" => "KwSeek",
        "self" => "KwSelf",
        "set" => "KwSet",
        "shl" => "KwShl",
        "shr" => "KwShr",
        "signal" => "KwSignal",
        "sizeof" => "KwSizeOf",
        "skip" => "KwSkip",
        "string" => "KwString",
        "tag" => "KwTag",
        "tell" => "KwTell",
        "then" => "KwThen",
        "timeout" => "KwTimeout",
        "to" => "KwTo",
        "true" => "KwTrue",
        "type" => "KwType",
        "unchecked" => "KwUnchecked",
        "union" => "KwUnion",
        "unit" => "KwUnit",
        "unqualified" => "KwUnqualified",
        "var" => "KwVar",
        "wait" => "KwWait",
        "when" => "KwWhen",
        "write" => "KwWrite",
        "xor" => "KwXor",
        "#if" => "PPKwIf",
        "#elseif" => "PPKwElseif",
        "#elsif" => "PPKwElsif",
        "#else" => "PPKwElse",
        "#end" => "PPKwEnd",
        "#endif" => "PPKwEndIf",
        "int_literal" => "IntLiteral",
        "real_literal" => "RealLiteral",
        "radix_literal" => "RadixLiteral",
        "char_literal" => "CharLiteral",
        "string_literal" => "StringLiteral",
        "identifier" => "Identifier",
        _ => unreachable!("{}", token),
    }
}

fn thing_to_method_name(token: &str) -> &str {
    match token {
        "@" => "at",
        "&" => "ampersand",
        "->" => "arrow",
        "^" => "caret",
        ":" => "colon",
        ":=" => "assign",
        "," => "comma",
        ".." => "range",
        "." => "dot",
        "=" => "equ",
        ">=" => "greater_equ",
        ">" => "greater",
        "#" => "pound",
        "=>" => "imply",
        "<=" => "less_equ",
        "(" => "l_paren",
        "<" => "less",
        "-" => "minus",
        "+" => "plus",
        "|" => "pipe",
        ")" => "r_paren",
        ";" => "semicolon",
        "/" => "slash",
        "*" => "star",
        "**" => "exp",
        "~" => "tilde",
        "addressint" => "addressint",
        "all" => "all",
        "and" => "and",
        "array" => "array",
        "asm" => "asm",
        "assert" => "assert",
        "begin" => "begin",
        "bind" => "bind",
        "bits" => "bits",
        "body" => "body",
        "boolean" => "boolean",
        "break" => "break",
        "by" => "by",
        "case" => "case",
        "char" => "char",
        "cheat" => "cheat",
        "checked" => "checked",
        "class" => "class",
        "close" => "close",
        "collection" => "collection",
        "condition" => "condition",
        "const" => "const",
        "decreasing" => "decreasing",
        "def" => "def",
        "deferred" => "deferred",
        "div" => "div",
        "elif" => "elif",
        "else" => "else",
        "elseif" => "elseif",
        "elsif" => "elsif",
        "end" => "end",
        "endcase" => "endcase",
        "endfor" => "endfor",
        "endif" => "endif",
        "endloop" => "endloop",
        "enum" => "enum",
        "exit" => "exit",
        "export" => "export",
        "external" => "external",
        "false" => "false",
        "flexible" => "flexible",
        "for" => "for",
        "fork" => "fork",
        "forward" => "forward",
        "free" => "free",
        "function" => "function",
        "get" => "get",
        "handler" => "handler",
        "if" => "if",
        "implement" => "implement",
        "import" => "import",
        "in" => "in",
        "include" => "include",
        "inherit" => "inherit",
        "init" => "init",
        "int" => "int",
        "int1" => "int1",
        "int2" => "int2",
        "int4" => "int4",
        "invariant" => "invariant",
        "label" => "label",
        "loop" => "loop",
        "mod" => "mod",
        "module" => "module",
        "monitor" => "monitor",
        "nat" => "nat",
        "nat1" => "nat1",
        "nat2" => "nat2",
        "nat4" => "nat4",
        "new" => "new",
        "nil" => "nil",
        "not" => "not",
        "objectclass" => "objectclass",
        "of" => "of",
        "opaque" => "opaque",
        "open" => "open",
        "or" => "or",
        "packed" => "packed",
        "pause" => "pause",
        "pervasive" => "pervasive",
        "pointer" => "pointer",
        "post" => "post",
        "pre" => "pre",
        "priority" => "priority",
        "procedure" => "procedure",
        "process" => "process",
        "put" => "put",
        "quit" => "quit",
        "read" => "read",
        "real" => "real",
        "real4" => "real4",
        "real8" => "real8",
        "record" => "record",
        "register" => "register",
        "rem" => "rem",
        "result" => "result",
        "return" => "return",
        "seek" => "seek",
        "self" => "self",
        "set" => "set",
        "shl" => "shl",
        "shr" => "shr",
        "signal" => "signal",
        "sizeof" => "sizeof",
        "skip" => "skip",
        "string" => "string",
        "tag" => "tag",
        "tell" => "tell",
        "then" => "then",
        "timeout" => "timeout",
        "to" => "to",
        "true" => "true",
        "type" => "type",
        "unchecked" => "unchecked",
        "union" => "union",
        "unit" => "unit",
        "unqualified" => "unqualified",
        "var" => "var",
        "wait" => "wait",
        "when" => "when",
        "write" => "write",
        "xor" => "xor",
        "#if" => "pp_if",
        "#elseif" => "pp_elseif",
        "#elsif" => "pp_elsif",
        "#else" => "pp_else",
        "#end" => "pp_end",
        "#endif" => "pp_endif",
        "int_literal" => "int_literal",
        "real_literal" => "real_literal",
        "radix_literal" => "radix_literal",
        "char_literal" => "char_literal",
        "string_literal" => "string_literal",
        "identifier" => "identifier",
        "Type" => "ty",
        _ => token,
    }
}
