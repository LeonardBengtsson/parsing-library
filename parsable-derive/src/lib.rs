use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

#[proc_macro_derive(Parsable, attributes(literal))]
pub fn parsable_derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    if !input.generics.params.empty_or_trailing() {
        return quote! { 
            compile_error!("`Parsable` cannot be derived for generic items") 
        }.into()
    }
    
    match input.data {
        syn::Data::Struct(data_struct) => struct_derive(input.ident, data_struct).into(),

        syn::Data::Enum(data_enum) => enum_derive(input.ident, data_enum).into(),
        
        syn::Data::Union(..) => quote! { 
            compile_error!("`Parsable` cannot be derived for unions") 
        }.into(),
    }
}

fn call_parse(tokens: TokenStream2, first_member: &mut bool) -> TokenStream2 {
    let inner = if *first_member {
        *first_member = false;
        quote! { <#tokens as parsable::Parsable>::parse(stream)? }
    } else {
        quote! { <#tokens as parsable::Parsable>::parse_or_error(stream) }
    };
    quote! { parsable::ok_or_throw!( #inner ) }
}

fn member_derive(ty: &syn::Type, first_member: &mut bool) -> TokenStream2 {
    match ty {
        #![cfg_attr(test, deny(non_exhaustive_omitted_patterns))]
        
        syn::Type::Macro(type_macro) => {
            call_parse(quote! { #type_macro }.into(), first_member)},
        syn::Type::Paren(type_paren) => {
            member_derive(&type_paren.elem, first_member)},
        syn::Type::Path(type_path) => {
            call_parse(quote! { #type_path }, first_member)},
        syn::Type::Tuple(type_tuple) => {
            let parses = type_tuple.elems.iter()
                .map(|ty| member_derive(ty, first_member));
            quote! { ( #( #parses ),* ) }
        },
        syn::Type::Array(type_array) => quote! { 
            compile_error!(format!("`Parsable` cannot be derived for array type {} (consider using `RepeatLimited`)", #type_array))},
        syn::Type::BareFn(type_bare_fn) => quote! { 
            compile_error!(format!("`Parsable` cannot be derived for bare function type {}", #type_bare_fn))},
        syn::Type::Group(type_group) => quote! {
            compile_error!(format!("`Parsable` cannot be derived for type group {}", #type_group))},
        syn::Type::ImplTrait(type_impl_trait) => quote! { 
            compile_error!(format!("`Parsable` cannot be derived for impl trait type {}", #type_impl_trait))},
        syn::Type::Infer(type_infer) => quote! {
            compile_error!(format!("`Parsable` derive could not infer type {}", #type_infer))},
        syn::Type::Never(..) => quote! { 
            compile_error!("`Parsable` cannot be derived for Never type")},
        syn::Type::Ptr(type_ptr) => quote! { 
            compile_error!(format!("`Parsable` cannot be derived for pointer type {}", #type_ptr))},
        syn::Type::Reference(type_reference) => quote! { 
            compile_error!(format!("`Parsable` cannot be derived for type reference {}", #type_reference))},
        syn::Type::Slice(type_slice) => quote! { 
            compile_error!(format!("`Parsable` cannot be derived for slice type {} (consider using `Repeat`)", #type_slice))},
        syn::Type::TraitObject(type_trait_object) => quote! { 
            compile_error!(format!("`Parsable` cannot be derived for trait object {}", #type_trait_object))},
        syn::Type::Verbatim(token_stream) => quote! {
            compile_error!(format!("`Parsable` derive could not infer type from {}", #token_stream))},
        ty => quote! { 
            compile_error!(format!("`Parsable` cannot be derived for {}", #ty))},
    }
}

fn impl_block(type_ident: &syn::Ident, inner_parse: TokenStream2) -> TokenStream2 {
    let type_name = &type_ident.to_string();
    quote! {
        impl<'__parsable_derive> parsable::Parsable<'__parsable_derive> for #type_ident {
            fn parse(stream: &mut parsable::ScopedStream<'__parsable_derive>) -> parsable::ParseOutcome<Self> {
                #![allow(unexpected_cfgs)]
                #[cfg(parsable_derive_debug)] {
                    println!("DEBUG >>>>>>>>>>> {}", #type_name);
                }
                let res = stream.scope(|stream| {
                    #inner_parse
                });
                #[cfg(parsable_derive_debug)] {
                    println!("DEBUG <<<<<<<<<<< {}\n{:?}", #type_name, &res);
                }
                res
            }

            fn error() -> parsable::ParseError {
                String::from(#type_name)
            }
        }
    }
}

fn field_derive(field: &syn::Field, first_member: &mut bool) -> TokenStream2 {
    let ty = &field.ty;
    let is_unit_type = is_unit_type(ty);

    let name = &field.ident;
    let prefix = match name {
        Some(name) => quote! { #name: },
        None => quote! {}
    };

    if let Some(literal) = get_literal_attribute(&field.attrs) {
        if is_unit_type {
            let literal_parse = if *first_member {
                *first_member = false;
                quote! { parsable::parse_literal(stream, #literal)? }
            } else {
                quote! { parsable::parse_literal_or_error(stream, #literal) }
            };
            quote! { #prefix parsable::ok_or_throw!( #literal_parse ) }
        } else {
            quote! { #prefix compile_error!("Unexpected `literal` attribute") }
        }
    } else {
        if is_unit_type {
            quote! { #prefix compile_error!("Expected `literal` attribute") }
        } else {
            let member = member_derive(ty, first_member);
            quote! { #prefix #member }
        }
    }
}

fn named_fields_derive(fields: &syn::FieldsNamed) -> Vec<TokenStream2> {
    let mut first_member = true;
    fields.named.iter().map(|field| field_derive(field, &mut first_member)).collect()
}

fn unnamed_fields_derive(fields: &syn::FieldsUnnamed) -> Vec<TokenStream2> {
    let mut first_member = true;
    fields.unnamed.iter().map(|field| field_derive(field, &mut first_member)).collect()
}

fn struct_derive(type_ident: syn::Ident, data_struct: syn::DataStruct) -> TokenStream2 {
    match data_struct.fields {
        syn::Fields::Named(fields) => {
            let fields = named_fields_derive(&fields);
            impl_block(
                &type_ident,
                quote! {
                    std::option::Option::Some(std::result::Result::Ok(Self {
                        #( #fields ),*
                    }))
                }
            ).into()
        },
        
        syn::Fields::Unnamed(fields) => {
            let fields = unnamed_fields_derive(&fields);
            impl_block(
                &type_ident,
                quote! {
                    std::option::Option::Some(std::result::Result::Ok(Self (
                        #( #fields ),*
                    )))
                }
            ).into()
        },
        
        syn::Fields::Unit => quote! { 
            compile_error!("`Parsable` cannot be derived for unit structs") 
        }.into(),
    }
}

fn enum_derive(enum_name: syn::Ident, data_enum: syn::DataEnum) -> TokenStream2 {
    let variants = data_enum.variants.iter().map(|variant| {
        let variant_name = &variant.ident;
        match &variant.fields {
            syn::Fields::Named(fields) => {
                let fields = named_fields_derive(fields);
                quote! {
                    Self::#variant_name {
                        #( #fields ),*
                    }
                }
            },
            syn::Fields::Unnamed(fields) => {
                let fields = unnamed_fields_derive(fields);
                quote! {
                    Self::#variant_name(
                        #( #fields ),*
                    )
                }
            },
            syn::Fields::Unit => {
                if let Some(literal) = get_literal_attribute(&variant.attrs) {
                    quote! { {
                        let _ = parsable::ok_or_throw!( parsable::parse_literal(stream, #literal)? );
                        Self::#variant_name
                    } }
                } else {
                    quote! { compile_error!("Expected `literal` attribute") }
                }
            },
        }
    });

    impl_block(
        &enum_name, 
        quote! {
            std::option::Option::None
                #( .or_else(|| std::option::Option::Some(std::result::Result::Ok( #variants ))) )*
        }
    ).into()
}

fn get_literal_attribute(attributes: &Vec<syn::Attribute>) -> Option<syn::LitByteStr> {
    attributes.iter().find_map(|attr| {
        let name_value = if let syn::Meta::NameValue(name_value) = &attr.meta
            { name_value } else { return None; };
        if !name_value.path.is_ident("literal") { return None; }
        
        let lit_expr = if let syn::Expr::Lit(lit_expr) = &name_value.value
            { lit_expr } else { return None; };
        
        if let syn::Lit::ByteStr(lit_str) = &lit_expr.lit {
            Some(lit_str.clone())
        } else {
            None
        }
    })
}

fn is_unit_type(ty: &syn::Type) -> bool {
    if let syn::Type::Tuple(tuple_type) = ty {
        tuple_type.elems.empty_or_trailing()
    } else {
        false
    }
}
