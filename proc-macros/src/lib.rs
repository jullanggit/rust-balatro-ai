use proc_macro::TokenStream;
use proc_macro2::{Delimiter, Group, Literal, Span};
use quote::{ToTokens, format_ident, quote};
use syn::{Data, DeriveInput, Fields, Ident, Index, Token, Type, parse::Parse, parse_macro_input};

#[proc_macro_derive(Serialize, attributes(serialize))]
pub fn derive_serialize(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = &input.ident;

    let mut serialize_fns = Vec::new();
    for attr in input
        .attrs
        .iter()
        .filter(|a| a.path().is_ident("serialize"))
    {
        if let Ok(list) = attr.parse_args_with(IdentList::parse) {
            serialize_fns.extend(list.0);
        }
    }

    let serialize_fn_types: Vec<_> = serialize_fns
        .iter()
        .map(|f| format_ident!("{}_type", f))
        .collect();

    let fns_generics = quote! {
        #(#serialize_fn_types: Serialize),*
    };
    let fns_args = quote! {
        #(#serialize_fns: fn(&#name) -> #serialize_fn_types),*
    };
    let get_fns_len = quote! {
        #[allow(non_camel_case_types, unused_variables)]
        const fn get_fns_len<#fns_generics>(#fns_args) -> usize {
            #(#serialize_fn_types::LEN + )* 0
        }
    };

    let encode_fns = quote! {
        // uses generics for the return types of the functions, to let the type system infer them
        #[allow(non_camel_case_types, unused_variables)]
        fn encode_fns<#fns_generics>
            (value: &#name, mut offset: usize, out: &mut [TensorElement; #name::LEN], #fns_args)
        {
            #(
                let len = #serialize_fn_types::LEN;

                out[offset..offset + len].copy_from_slice(&Serialize::serialize(&value.#serialize_fns()));

                offset += len;
            )*
        }
    };

    match input.data {
        Data::Struct(dstruct) => {
            macro_rules! box_dyn {
                ($e:expr) => {
                    Box::new($e) as Box<dyn ToTokens>
                };
            }
            let ([field_constructors, field_accessors], delimiter): (
                [Vec<Box<dyn ToTokens>>; 2],
                _,
            ) = match dstruct.fields.clone() {
                Fields::Named(fields_named) => {
                    let fields: Vec<_> = fields_named
                        .named
                        .iter()
                        .map(|field| box_dyn!(field.ident.clone().unwrap()))
                        .collect();
                    (
                        [
                            fields
                                .iter()
                                .map(|field| box_dyn!(quote! {#field:}))
                                .collect(),
                            fields,
                        ],
                        Delimiter::Brace,
                    )
                }
                Fields::Unnamed(fields_unnamed) => {
                    let fields: Vec<_> = fields_unnamed
                        .unnamed
                        .iter()
                        .enumerate()
                        .map(|(i, _)| {
                            box_dyn!(Index {
                                index: i as u32,
                                span: Span::call_site(),
                            })
                        })
                        .collect();
                    (
                        [fields.iter().map(|_| box_dyn!(quote! {})).collect(), fields],
                        Delimiter::Parenthesis,
                    )
                }
                Fields::Unit => ([Vec::new(), Vec::new()], Delimiter::None),
            };
            let types: Vec<Type> = match dstruct.fields {
                Fields::Named(fields_named) => fields_named
                    .named
                    .iter()
                    .map(|field| field.ty.clone())
                    .collect(),
                Fields::Unnamed(fields_unnamed) => fields_unnamed
                    .unnamed
                    .iter()
                    .map(|field| field.ty.clone())
                    .collect(),
                Fields::Unit => Vec::new(),
            };

            let constructor = Group::new(
                delimiter,
                quote! {
                    #(
                        #field_constructors {
                            let len = <#types as Serialize>::LEN;
                            let val = <#types>::deserialize(&value[offset..offset + len].try_into().unwrap())?;
                            offset += len;

                            val
                        },
                    )*
                },
            );

            TokenStream::from(quote! {
                #[automatically_derived]
                impl Serialize for #name {
                    const LEN: usize = {
                        #get_fns_len
                        #(<#types as Serialize>::LEN + )* 0 + get_fns_len(#(#name::#serialize_fns),*)
                    };

                    fn serialize(&self) -> [TensorElement; Self::LEN] {
                        let mut initial = [0.0; _];

                        let mut offset = 0;

                        #(
                            let len = <#types as Serialize>::LEN;
                            initial[offset..offset + len]
                                    .copy_from_slice(&<#types as Serialize>::serialize(&self.#field_accessors));

                            offset += len;
                        )*

                        #encode_fns
                        encode_fns(self, offset, &mut initial, #(#name::#serialize_fns),*);

                        initial
                    }
                    fn deserialize(value: &[TensorElement; Self::LEN]) -> Option<Self> {
                        let mut offset = 0;

                        Some(Self #constructor )
                    }
                }
            })
        }
        Data::Enum(denum) => {
            let variants: Vec<Ident> = denum
                .variants
                .iter()
                .map(|variant| variant.ident.clone())
                .collect();
            let nums: Vec<Literal> = (0..variants.len()).map(Literal::usize_unsuffixed).collect();
            let fields: Vec<Vec<Ident>> = denum
                .variants
                .iter()
                .map(|variant| {
                    variant
                        .fields
                        .iter()
                        .enumerate()
                        .map(|(i, field)| field.ident.clone().unwrap_or(format_ident!("_{i}")))
                        .collect()
                })
                .collect();
            let types: Vec<Vec<Type>> = denum
                .variants
                .iter()
                .map(|variant| {
                    variant
                        .fields
                        .iter()
                        .map(|field| field.ty.clone())
                        .collect()
                })
                .collect();
            let max_len = quote! {
                const fn max_len() -> usize {
                    #[allow(unused_mut)]
                    let mut max = 0;

                    #(
                        let len = #(<#types as Serialize>::LEN + )* 0;
                            if len > max {
                                max = len;
                            }
                    )*

                    max
                }
            };
            let variant_count = quote! {
                core::mem::variant_count::<#name>()
            };
            let fields_match: Vec<_> = fields
                .iter()
                .map(|fields| {
                    if fields.is_empty() {
                        quote! {}
                    } else {
                        quote! {
                            (#(#fields),*)
                        }
                    }
                })
                .collect();

            let fields_return_serialize: Vec<_> = fields
                .iter()
                .zip(&types)
                .map(|(fields, types)| {
                    if fields.is_empty() {
                        quote! {}
                    } else {
                        quote! {
                            #(
                                initial[offset..offset+<#types as Serialize>::LEN]
                                    .copy_from_slice(&<#types as Serialize>::serialize(#fields));
                            )*
                        }
                    }
                })
                .collect();

            let fields_return_deserialize: Vec<_> = fields
                .iter()
                .zip(types)
                .map(|(fields, types)| {
                    if fields.is_empty() {
                        quote! {}
                    } else {
                        quote! {
                            (
                                #(
                                    <#types as Serialize>::deserialize(
                                        &value[offset..offset + <#types>::LEN]
                                            .try_into()
                                            .expect("Lengths should match")
                                    )?
                                ),*
                            )
                        }
                    }
                })
                .collect();

            TokenStream::from(quote! {
                #[automatically_derived]
                impl Serialize for #name {
                    const LEN: usize = {
                        #max_len
                        #get_fns_len

                        #variant_count + max_len() + get_fns_len(#(#name::#serialize_fns),*)
                    };

                    #[allow(non_snake_case)]
                    fn serialize(&self) -> [TensorElement; Self::LEN] {
                        let mut initial = [0.0; _];

                        #[allow(unused_mut)]
                        let mut offset = #variant_count;

                        match self {
                            #(
                                #name::#variants #fields_match => {
                                    initial[#nums] = 1.0;

                                    #fields_return_serialize
                                }
                            )*
                        }

                        #max_len

                        #encode_fns
                        encode_fns(self, offset + max_len(), &mut initial, #(#name::#serialize_fns),*);

                        initial
                    }
                    fn deserialize(value: &[TensorElement; Self::LEN]) -> Option<Self> {
                        let offset = #variant_count;
                        let mut i = 0;

                        #(
                            if value[i] == 1. {
                                return Some(
                                    #name::#variants #fields_return_deserialize
                                )
                            }
                            i += 1;
                        )*

                        None
                    }
                }
            })
        }
        Data::Union(_) => {
            syn::Error::new_spanned(&input.ident, "Serialize derive does not support unions")
                .into_compile_error()
                .into()
        }
    }
}

struct IdentList(Vec<Ident>);
impl Parse for IdentList {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        Ok(IdentList(
            input
                .parse_terminated(Ident::parse, Token![,])?
                .into_iter()
                .collect(),
        ))
    }
}
