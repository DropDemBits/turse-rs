use std::{collections::BTreeSet, fs, str::FromStr};

use heck::{ToPascalCase, ToSnekCase};
use miette::IntoDiagnostic;
use proc_macro2::TokenStream;
use quote::{format_ident, quote, ToTokens};
use stackful_spec::{BytecodeSpec, PropertyKind, PropertyValue, Type};

use crate::{flags, util::project_root};

use super::{add_preamble, ensure_file_contents, reformat};

pub(crate) fn codegen(check: bool) -> miette::Result<()> {
    let spec_path = "spec/turing-bytecode/spec.kdl";
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

    let encode_defs = generate_encode_defs(&spec)?;
    let encode_defs_file =
        project_root().join("spec/turing-bytecode/src/encode/instruction/generated.rs");
    ensure_file_contents(
        flags::CodegenType::TuringBytecode,
        encode_defs_file.as_std_path(),
        &encode_defs,
        check,
    );

    // Print operator coverage for progress tracking, but only while we're under the known opcode limit
    let opcode_count = spec.instructions.len();
    let max_opcodes = 255usize;

    if opcode_count < max_opcodes {
        let coverage = (opcode_count as f64) / (max_opcodes as f64) * 100.0;
        eprintln!("operator coverage: {coverage:.2}% ({opcode_count}/{max_opcodes})")
    }

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
            Type::Scalar(ty) => {
                let doc = doc_comment(ty.description());
                let ident = format_ident!("{}", ty.name().to_pascal_case());
                let repr_ty = format_ident!(
                    "{}",
                    ty.repr_type().expect("all scalars must have repr types")
                );

                quote! {
                    #doc
                    pub type #ident = #repr_ty;
                }
            }
            Type::Struct(ty) => {
                let doc = doc_comment(ty.description());
                let ident = format_ident!("{}", ty.name().to_pascal_case());
                let size = ty.size() as usize;

                let fields: Vec<_> = ty
                    .fields()
                    .iter()
                    .map(|field| {
                        let doc = doc_comment(field.description());
                        let ident = format_ident!("{}", field.name().to_snek_case());
                        let ty_name = format_ident!("{}", field.ty().to_pascal_case());

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
                        #[doc = "Fixed (i.e. non-dynamic) size of the type, in bytes."]
                        pub const fn fixed_size() -> usize {
                            #size
                        }

                        #[doc = "Size of the type, in bytes."]
                        pub fn size(&self) -> usize {
                            #size
                        }
                    }
                }
            }
            Type::Enum(ty) => {
                let doc = doc_comment(ty.description());
                let ident = format_ident!("{}", ty.name().to_pascal_case());
                let repr_ty = format_ident!(
                    "{}",
                    ty.repr_type().expect("all enums must have repr types")
                );
                let size = ty.size() as usize;

                let variants: Vec<_> = ty
                    .variants()
                    .iter()
                    .map(|variant| {
                        let doc = doc_comment(variant.description());
                        let ident = format_ident!("{}", variant.name().to_pascal_case());
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

                        let ident = format_ident!("{}", prop_name.to_snek_case());
                        let prop_ty = match prop_kind {
                            PropertyKind::String => quote! { &'static str },
                            PropertyKind::Number => {
                                if values.iter().any(|(_, value)| matches!(value, PropertyValue::Number(n) if n.is_negative())) {
                                    quote! { isize }
                                } else {
                                    quote! { usize }
                                }
                            },
                            PropertyKind::Bool => quote! { bool },
                            _ => unimplemented!("unhandled property type {prop_kind:?}"),
                        };

                        let arms: Vec<_> = values
                            .into_iter()
                            .map(|(variant, value)| {
                                let ident = format_ident!("{}", variant.to_pascal_case());
                                let value = match value {
                                    PropertyValue::String(v) => quote! { #v },
                                    PropertyValue::Number(v) => {
                                        proc_macro2::Literal::from_str(&format!("{}{prop_ty}", v))
                                            .expect("property type number should be valid")
                                            .to_token_stream()
                                    }
                                    PropertyValue::Bool(v) => quote! { #v },
                                    _ => todo!(),
                                };

                                quote! { Self::#ident => #value }
                            })
                            .collect();

                        accessors.push(quote! {
                            pub fn #ident(&self) -> #prop_ty {
                                match self {
                                    #(#arms,)*
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
                        #[doc = "Fixed (i.e. non-dynamic) size of the type, in bytes."]
                        pub const fn fixed_size() -> usize {
                            #size
                        }

                        #[doc = "Size of the type, in bytes."]
                        pub fn size(&self) -> usize {
                            #size
                        }

                        #(#property_accessors)*
                    }
                }
            }
            Type::Union(ty) => {
                let doc = doc_comment(ty.description());
                let ident = format_ident!("{}", ty.name().to_pascal_case());
                let repr_ty = format_ident!(
                    "{}",
                    ty.repr_type().expect("all unions must have repr types")
                );
                let tag_size = ty.tag_size() as usize;

                let variants: Vec<_> = ty
                    .variants()
                    .iter()
                    .map(|variant| {
                        let doc = doc_comment(variant.description());
                        let ident = format_ident!("{}", variant.name().to_pascal_case());
                        let ordinal = proc_macro2::Literal::from_str(&format!(
                            "{}{repr_ty}",
                            variant.ordinal()
                        ))
                        .expect("should be a valid literal");

                        let fields: Vec<_> = variant
                            .fields()
                            .iter()
                            .map(|field| {
                                let doc = doc_comment(field.description());
                                let ident = format_ident!("{}", field.name().to_snek_case());
                                let ty_name = format_ident!("{}", field.ty().to_pascal_case());

                                quote! {
                                    #doc
                                    #ident: #ty_name
                                }
                            })
                            .collect();
                        // Don't let empty field variants have unnecessary braces
                        let field_group = if !fields.is_empty() {
                            quote!{ { #(#fields),* } }
                        } else {
                           quote!{} 
                        };

                        quote! {
                            #doc
                            #ident #field_group = #ordinal
                        }
                    })
                    .collect();

                let variant_size_arms: Vec<_> = ty.variants().iter().map(|variant| {
                    let variant_size = proc_macro2::Literal::usize_unsuffixed(tag_size + variant.size() as usize);
                    let ident = format_ident!("{}", variant.name().to_pascal_case());
                    let field_group = if !variant.fields().is_empty() {
                        quote!{ { .. } }
                    } else {
                       quote!{} 
                    };

                    quote!{ Self::#ident #field_group => #variant_size }
                }).collect();

                quote! {
                    #doc
                    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
                    #[repr(#repr_ty)]
                    pub enum #ident {
                        #(#variants,)*
                    }

                    impl #ident {
                        #[doc = "Size of the type, in bytes.\nIncludes the size of the tag, and may vary in size depending on the variant."]
                        pub fn size(&self) -> usize {
                            match self {
                                #(#variant_size_arms,)*
                            }
                        }
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

/// Generates definitions used during encoding.
fn generate_encode_defs(spec: &BytecodeSpec) -> miette::Result<String> {
    let operands = generate_encode_operands(spec);
    let ty_encodes = generate_encode_impls(spec);
    let instr_encodes = generate_instr_encode_fns(spec);

    Ok(add_preamble(
        flags::CodegenType::TuringBytecode,
        reformat(
            quote! {
               use byteorder::{WriteBytesExt, LE};

               use crate::instruction::*;
               use crate::encode::instruction::{Instruction, InstructionRef, InstructionEncoder};

               #operands
               #ty_encodes
               #instr_encodes
            }
            .to_string(),
        )?,
    ))
}

fn generate_encode_operands(spec: &BytecodeSpec) -> TokenStream {
    let max_operand_count = spec
        .instructions
        .iter()
        .map(|instr| instr.immediate_operands().len())
        .max()
        .unwrap_or(0);

    let operand_tys: BTreeSet<_> = spec
        .instructions
        .iter()
        .flat_map(|instr| instr.immediate_operands())
        .map(|operand| operand.ty)
        .collect();
    let operand_tys: Vec<_> = operand_tys.into_iter().map(|ty| &spec.types[ty]).collect();

    let variants: Vec<_> = operand_tys
        .iter()
        .map(|ty| {
            let ident = format_ident!("{}", ty.name().to_pascal_case());

            quote! { #ident(#ident) }
        })
        .collect();

    let sizes: Vec<_> = operand_tys
        .iter()
        .map(|ty| {
            let ident = format_ident!("{}", ty.name().to_pascal_case());
            // Instruction operands are always aligned to the next 4-byte boundary.
            let size = proc_macro2::Literal::usize_suffixed(ty.size().next_multiple_of(4) as usize);

            quote! { Self::#ident(_) => #size }
        })
        .collect();

    let encodes: Vec<_> = operand_tys
        .iter()
        .map(|ty| {
            let ident = format_ident!("{}", ty.name().to_pascal_case());
            let encode = encode_type(ty, quote! { value }, format_ident!("out"), true);

            if let Some(padding) = ty
                .size()
                .next_multiple_of(4)
                .checked_sub(ty.size())
                .filter(|padding| *padding > 0)
            {
                // Operands are always aligned to 4-byte boundaries, pad up to the nearest one.
                let padding: Vec<_> = (0..padding)
                    .into_iter()
                    .map(|_| quote! { out.write_u8(0) })
                    .collect();

                quote! {
                    Self::#ident(value) => {
                        #encode?;
                        #(#padding?;)*

                        Ok(())
                    }
                }
            } else {
                // No padding required, can emit as-is
                quote! { Self::#ident(value) => #encode }
            }
        })
        .collect();

    quote! {
        #[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
        #[doc = "An operand for an encoded instruction."]
        pub enum Operand {
            #(#variants),*
        }

        impl Operand {
            #[doc = "Size of the operand, in bytes."]
            pub fn size(&self) -> usize {
                match self {
                    #(#sizes,)*
                }
            }

            #[doc = "Encodes the operand into the equivalent byte representation."]
            pub fn encode(&self, out: &mut impl std::io::Write) -> std::io::Result<()> {
                match self {
                    #(#encodes,)*
                }
            }
        }

        pub(crate) const MAX_OPERANDS: usize = #max_operand_count;
    }
}

fn generate_encode_impls(spec: &BytecodeSpec) -> TokenStream {
    let encode_impls = spec.types.types.iter().map(|ty| {
        let ident = format_ident!("{}", ty.name().to_pascal_case());

        match ty {
            Type::Scalar(_) => quote! {},
            Type::Struct(ty) => {
                let field_encodes: Vec<_> = ty
                    .fields()
                    .iter()
                    .map(|field| {
                        let ty = spec
                            .types
                            .get(field.ty())
                            .expect("struct field should have valid type");
                        let ty = &spec.types[ty];
                        let field_ident = format_ident!("{}", field.name());
                        let encode = encode_type(
                            ty,
                            quote! { self.#field_ident },
                            format_ident!("out"),
                            false,
                        );

                        quote! { #encode?; }
                    })
                    .collect();

                quote! {
                    impl #ident {
                        #[doc = "Encodes the type into the equivalent byte representation."]
                        pub fn encode(&self, out: &mut impl std::io::Write) -> std::io::Result<()> {
                            #(#field_encodes)*

                            Ok(())
                        }
                    }
                }
            }
            Type::Enum(ty) => {
                let repr_type = ty
                    .repr_type()
                    .expect("all enum types should have repr types");
                let as_repr = format_ident!("{repr_type}");
                let write_ident = format_ident!("write_{repr_type}");
                let as_le = if matches!(repr_type, "u8" | "i8") {
                    quote! {}
                } else {
                    quote! {LE}
                };

                quote! {
                    impl #ident {
                        #[doc = "Encodes the type into the equivalent byte representation."]
                        pub fn encode(&self, out: &mut impl std::io::Write) -> std::io::Result<()> {
                            out.#write_ident::<#as_le>(*self as #as_repr)
                        }
                    }
                }
            }
            Type::Union(ty) => {
                let tag_type = ty
                    .repr_type()
                    .expect("all union types should have repr types");
                let write_tag_ident = format_ident!("write_{tag_type}");
                let tag_as_le = if matches!(tag_type, "u8" | "i8") {
                    quote! {}
                } else {
                    quote! {LE}
                };

                let variant_encodes: Vec<_> = ty.variants().iter().map(|variant| {
                    let ident = format_ident!("{}", variant.name().to_pascal_case());
                    let ordinal = proc_macro2::Literal::from_str(&format!(
                        "{}{tag_type}",
                        variant.ordinal()
                    ))
                    .expect("should be a valid literal");

                    let field_names: Vec<_> = variant
                        .fields()
                        .iter()
                        .map(|field| {
                            format_ident!("{}", field.name().to_snek_case())
                        })
                        .collect();
                    let field_pattern = if !field_names.is_empty() {
                        quote! { { #(#field_names),*} }
                    } else {
                        quote!{ }
                    };

                    let field_encodes: Vec<_> = variant
                        .fields()
                        .iter()
                        .map(|field| {
                            let ty = spec
                                .types
                                .get(field.ty())
                                .expect("union variant field should have valid type");
                            let ty = &spec.types[ty];
                            let field_ident = format_ident!("{}", field.name());
                            let encode = encode_type(
                                ty,
                                quote! { #field_ident },
                                format_ident!("out"),
                                true,
                            );

                            quote! { #encode?; }
                        })
                        .collect();

                    quote! {
                        Self::#ident #field_pattern => {
                            out.#write_tag_ident::<#tag_as_le>(#ordinal)?;
                            #(#field_encodes)*
                        }
                    }
                }).collect();

                quote! {
                    impl #ident {
                        #[doc = "Encodes the type into the equivalent byte representation."]
                        pub fn encode(&self, out: &mut impl std::io::Write) -> std::io::Result<()> {
                            match self {
                                #(#variant_encodes)*
                            }

                            Ok(())
                        }
                    }
                }
            }
            _ => unimplemented!("unhandled encoding type {ty:?}"),
        }
    });

    quote! { #(#encode_impls)* }
}

fn encode_type(
    ty: &Type,
    value_expr: TokenStream,
    out_ident: proc_macro2::Ident,
    deref: bool,
) -> TokenStream {
    match ty {
        Type::Scalar(ty) => {
            let repr_type = ty
                .repr_type()
                .expect("all scalar types should have repr types");
            let write_ident = format_ident!("write_{repr_type}");
            let as_le = if matches!(repr_type, "u8" | "i8") {
                quote! {}
            } else {
                quote! {LE}
            };

            if deref {
                quote! { #out_ident.#write_ident::<#as_le>(*#value_expr) }
            } else {
                quote! { #out_ident.#write_ident::<#as_le>(#value_expr) }
            }
        }
        Type::Struct(_) | Type::Enum(_) | Type::Union(_) => {
            quote! { #value_expr.encode(#out_ident) }
        }
        _ => unimplemented!("unhandled encoding type {ty:?}"),
    }
}

fn generate_instr_encode_fns(spec: &BytecodeSpec) -> TokenStream {
    fn escape_reserved(name: String) -> String {
        if matches!(name.as_str(), "return" | "add" | "for" | "if" | "break") {
            format!("{name}_")
        } else {
            name
        }
    }

    let instr_fns: Vec<_> = spec
        .instructions
        .iter()
        .map(|instr| {
            let docs = {
                let mut doc = format!(
                    "Encode a [**{0}**](Opcode::{0}) instruction.",
                    instr.mnemonic()
                );

                if !instr.immediate_operands().is_empty() {
                    doc.push_str("\n\n## Operands\n\n");

                    for operand in instr.immediate_operands() {
                        if let Some(description) = &operand.description {
                            doc.push_str(&format!(
                                "- {}: {description}\n",
                                operand.name.to_snek_case(),
                            ));
                        }
                    }
                }

                doc
            };
            let fn_name = format_ident!("{}", escape_reserved(instr.mnemonic().to_lowercase()));
            let variant_name = format_ident!("{}", instr.mnemonic());

            let params: Vec<_> = instr
                .immediate_operands()
                .iter()
                .map(|operand| {
                    let ident = format_ident!("{}", operand.name.to_snek_case());
                    let ty = format_ident!("{}", &spec.types[operand.ty].name().to_pascal_case());

                    quote! { #ident: #ty }
                })
                .collect();
            let operand_pushes: Vec<_> = instr
                .immediate_operands()
                .iter()
                .map(|operand| {
                    let ident = format_ident!("{}", operand.name.to_snek_case());
                    let ty = format_ident!("{}", &spec.types[operand.ty].name().to_pascal_case());

                    quote! { with_operand(Operand::#ty(#ident)) }
                })
                .collect();

            quote! {
                #[doc = #docs]
                pub fn #fn_name(&mut self, #(#params),*) -> InstructionRef {
                    self.add(Instruction::new(Opcode::#variant_name) #(.#operand_pushes)*)
                }
            }
        })
        .collect();

    quote! {
        impl InstructionEncoder {
            #(#instr_fns)*
        }
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
