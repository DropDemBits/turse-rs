use std::{collections::BTreeSet, fs, str::FromStr};

use miette::IntoDiagnostic;
use proc_macro2::TokenStream;
use quote::{format_ident, quote, ToTokens};
use stackful_spec::BytecodeSpec;

use crate::{flags, util::project_root};

use super::{add_preamble, ensure_file_contents, reformat};

// todo!
pub(crate) fn codegen(check: bool) -> miette::Result<()> {
    let spec_path = "spec/turing-bytecode/src/spec.kdl";
    let spec = fs::read_to_string(project_root().join(spec_path)).into_diagnostic()?;
    let spec: BytecodeSpec = spec.parse().map_err(|err| {
        miette::Report::from(err).with_source_code(miette::NamedSource::new(spec_path, spec))
    })?;

    let instr_defs = generate_instruction_defs(&spec)?;
    let instr_defs_file = project_root().join("spec/turing-bytecode/src/instruction/generated.rs");
    ensure_file_contents(
        flags::CodegenType::TuringBytecode,
        instr_defs_file.as_std_path(),
        &instr_defs,
        check,
    );

    Ok(())
}

/// Generates shared instruction definitions
fn generate_instruction_defs(spec: &BytecodeSpec) -> miette::Result<String> {
    let opcodes = generate_opcodes(spec);
    let types = generate_types(spec);

    Ok(add_preamble(
        flags::CodegenType::TuringBytecode,
        reformat(
            quote! {
                #opcodes
                #types
            }
            .to_string(),
        )?,
    ))
}

fn generate_opcodes(spec: &BytecodeSpec) -> TokenStream {
    let variants: Vec<_> = spec
        .instructions
        .iter()
        .map(|instr| {
            let docs = {
                let mut docs = String::new();

                if let Some(heading) = instr.heading() {
                    docs.push_str(heading);
                    docs.push_str("\n\n");
                }

                if let Some(description) = instr.description() {
                    docs.push_str(description);
                }

                doc_comment(Some(docs.as_str()))
            };

            let mnemonic = format_ident!("{}", instr.mnemonic());
            let opcode =
                proc_macro2::Literal::from_str(&format!("0x{:X}", instr.opcode())).unwrap();
            quote! {
                #docs
                #mnemonic = #opcode
            }
        })
        .collect();

    quote! {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        #[repr(u32)]
        #[doc = "An opcode for an instruction."]
        pub enum Opcode {
            #(#variants),*
        }
    }
}

fn generate_types(spec: &BytecodeSpec) -> TokenStream {
    let types: Vec<_> = spec
        .types
        .types
        .iter()
        .map(|ty| match ty {
            stackful_spec::Type::Scalar(ty) => {
                let doc = doc_comment(ty.description());
                let ident = format_ident!("{}", heck::AsPascalCase(ty.name()).to_string());
                let repr_ty = format_ident!(
                    "{}",
                    ty.repr_type().expect("all scalars must have repr types")
                );

                quote! {
                    #doc
                    pub type #ident = #repr_ty;
                }
            }
            stackful_spec::Type::Struct(ty) => {
                let doc = doc_comment(ty.description());
                let ident = format_ident!("{}", heck::AsPascalCase(ty.name()).to_string());
                let size = ty.size() as usize;

                let fields: Vec<_> = ty
                    .fields()
                    .iter()
                    .map(|field| {
                        let doc = doc_comment(field.description());
                        let ident = format_ident!("{}", heck::AsSnekCase(field.name()).to_string());
                        let ty_name =
                            format_ident!("{}", heck::AsPascalCase(field.ty()).to_string());

                        quote! {
                            #doc
                            pub #ident: #ty_name
                        }
                    })
                    .collect();

                quote! {
                    #doc
                    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
                    pub struct #ident {
                        #(#fields),*
                    }

                    impl #ident {
                        #[doc = "Size of the type, in bytes."]
                        pub fn size(&self) -> usize {
                            #size
                        }
                    }
                }
            }
            stackful_spec::Type::Enum(ty) => {
                let doc = doc_comment(ty.description());
                let ident = format_ident!("{}", heck::AsPascalCase(ty.name()).to_string());
                let repr_ty = format_ident!(
                    "{}",
                    ty.repr_type().expect("all scalars must have repr types")
                );
                let size = ty.size() as usize;

                let variants: Vec<_> = ty
                    .variants()
                    .iter()
                    .map(|variant| {
                        let doc = doc_comment(variant.description());
                        let ident =
                            format_ident!("{}", heck::AsPascalCase(variant.name()).to_string());
                        let ordinal = proc_macro2::Literal::from_str(&format!(
                            "{}{repr_ty}",
                            variant.ordinal()
                        ))
                        .expect("should be a valid literal");

                        quote! {
                            #doc
                            #ident = #ordinal
                        }
                    })
                    .collect();

                let shared_props: BTreeSet<_> = ty
                    .variants()
                    .iter()
                    .flat_map(|variant| variant.properties())
                    .map(|(name, prop)| (name, prop.kind()))
                    .collect();

                // FIXME: Really need property field definitions
                let property_accessors = if !shared_props.is_empty() {
                    let mut accessors = vec![];

                    for (prop_name, prop_kind) in shared_props {
                        let values: Vec<_> = ty
                            .variants()
                            .iter()
                            .map(|variant| {
                                (variant.name(), variant.property(prop_name).unwrap().value())
                            })
                            .collect();

                        let ident = format_ident!("{}", heck::AsSnekCase(prop_name).to_string());
                        let prop_ty = match prop_kind {
                            stackful_spec::PropertyKind::String => quote! { &'static str },
                            stackful_spec::PropertyKind::Number => quote! { usize },
                            stackful_spec::PropertyKind::Bool => quote! { bool },
                            _ => unimplemented!("unhandled property type {prop_kind:?}"),
                        };

                        let arms: Vec<_> = values
                            .into_iter()
                            .map(|(variant, value)| {
                                let ident =
                                    format_ident!("{}", heck::AsPascalCase(variant).to_string());
                                let value = match value {
                                    stackful_spec::PropertyValue::String(v) => quote! { #v },
                                    stackful_spec::PropertyValue::Number(v) => {
                                        proc_macro2::Literal::from_str(&format!("{}{prop_ty}", v))
                                            .expect("property type number should be valid")
                                            .to_token_stream()
                                    }
                                    stackful_spec::PropertyValue::Bool(v) => quote! { #v },
                                    _ => todo!(),
                                };

                                quote! { Self::#ident => #value, }
                            })
                            .collect();

                        accessors.push(quote! {
                            pub fn #ident(&self) -> #prop_ty {
                                match self {
                                    #(#arms)*
                                }
                            }
                        });
                    }

                    accessors
                } else {
                    vec![]
                };

                quote! {
                    #doc
                    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
                    #[repr(#repr_ty)]
                    pub enum #ident {
                        #(#variants),*
                    }

                    impl #ident {
                        #[doc = "Size of the type, in bytes."]
                        pub fn size(&self) -> usize {
                            #size
                        }

                        #(#property_accessors)*
                    }
                }
            }
            _ => unimplemented!("unhandled type kind {ty:?}"),
        })
        .collect();

    quote! {
        #(#types)*
    }
}

fn doc_comment(s: Option<&str>) -> TokenStream {
    match s {
        Some(s) if !s.is_empty() => quote! { #[doc = #s ]},
        _ => quote! {},
    }
}

#[test]
fn test() {
    codegen(true).unwrap();
}
