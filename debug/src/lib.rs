use proc_macro::TokenStream;
use quote::{quote, quote_spanned, ToTokens};
use syn::{self, spanned::Spanned};

struct CompileError<'a> {
    message: &'a str,
    span: proc_macro2::Span,
}

impl<'a> CompileError<'a> {
    fn to_token(&self) -> proc_macro2::TokenStream {
        let message = self.message;
        quote_spanned! { self.span => compile_error!(#message) }
    }
}

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(input as syn::DeriveInput);
    let name = input.ident;
    let name_str = syn::LitStr::new(&name.to_string(), name.span());

    let fields = if let syn::Data::Struct(data) = input.data {
        data.fields
    } else {
        return quote! { compile_error!("Expected struct input") }.into();
    };

    let named_fields: Vec<syn::Field> = if let syn::Fields::Named(f) = fields {
        f.named.into_iter().collect()
    } else {
        return quote! { compile_error!("Expected named fields") }.into();
    };

    let excluded_generics = named_fields
        .iter()
        .filter_map(extract_phantom_data_generic)
        .collect();

    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    let debug_fields = named_fields.iter().map(create_debug_field_impl);

    let inner_types = named_fields.iter().filter_map(get_inner_type);
    println!("{}", inner_types.clone().collect::<Vec<_>>().len());

    // FIXME: generics, as inner types, should comme from the attributes.
    // I can get the generic names from the struct and check if some attributes
    // TypePath start with it.
    let generics = get_generic_types(&input.generics, excluded_generics);

    let new_where_clause = if generics.is_empty() {
        None
    } else {
        Some(quote! { where
        #(#generics: std::fmt::Debug,)*
        #(#inner_types: std::fmt::Debug,)* })
    };

    quote! {
      impl #impl_generics std::fmt::Debug for #name #ty_generics #new_where_clause {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
          f.debug_struct(#name_str)
          #(#debug_fields)*
          .finish()
        }
      }
    }
    .into()
}

fn get_inner_type(field: &syn::Field) -> Option<&syn::Type> {
    let type_path = if let syn::Type::Path(ref tp) = field.ty {
        tp
    } else {
        return None;
    };

    let arg = &type_path.path.segments.last()?.arguments;

    let args_in_brackets = if let syn::PathArguments::AngleBracketed(a) = arg {
        a
    } else {
        return None;
    };

    let first = args_in_brackets.args.first()?;

    let generic_type = if let syn::GenericArgument::Type(t) = first {
        t
    } else {
        return None;
    };

    if let syn::Type::Path(generic_type_path) = generic_type {
        let segments = &generic_type_path.path.segments;
        if segments.len() == 2 {
            Some(generic_type)
        } else {
            return None;
        }
    } else {
        None
    }
}

fn extract_phantom_data_generic(f: &syn::Field) -> Option<&syn::TypePath> {
    let path_type = if let syn::Type::Path(pt) = &f.ty {
        pt
    } else {
        return None;
    };

    let type_ident = &path_type
        .path
        .segments
        .last()
        .expect("Path should have a last segmenbt")
        .ident;

    if type_ident != "PhantomData" {
        return None;
    };

    let arguments = &path_type
        .path
        .segments
        .last()
        .expect("PhantomData should have a last segment")
        .arguments;

    let generic_arg = if let syn::PathArguments::AngleBracketed(a) = arguments {
        a.args
            .first()
            .expect("PhantomData should have a first argument")
    } else {
        return None;
    };

    let generic_type = if let syn::GenericArgument::Type(t) = generic_arg {
        t
    } else {
        return None;
    };

    if let syn::Type::Path(p) = generic_type {
        Some(p)
    } else {
        return None;
    }
}

// FIXME: wrong implementation, train bounds belong in 'where' clause
fn add_trait_bounds(mut generics: syn::Generics, exclude: Vec<&syn::TypePath>) -> syn::Generics {
    for param in &mut generics.params {
        if let syn::GenericParam::Type(ref mut type_param) = *param {
            if exclude
                .iter()
                .filter_map(|t| t.path.get_ident())
                .all(|ident| ident.to_string() != type_param.ident.to_string())
            {
                type_param.bounds.push(syn::parse_quote!(std::fmt::Debug));
            }
        }
    }
    generics
}

fn get_generic_types(generics: &syn::Generics, exclude: Vec<&syn::TypePath>) -> Vec<syn::Ident> {
    generics
        .params
        .iter()
        .filter_map(|x| {
            if let syn::GenericParam::Type(type_param) = x {
                Some(&type_param.ident)
            } else {
                None
            }
        })
        .filter(|&f| {
            exclude
                .iter()
                .filter_map(|t| t.path.get_ident())
                .all(|ident| ident.to_string() != f.to_string())
        })
        .map(|x| x.to_owned())
        .collect()
}

fn extract_format_string(attr: &syn::Attribute) -> Result<syn::LitStr, CompileError> {
    let meta = if let syn::Meta::NameValue(m) = &attr.meta {
        m
    } else {
        return Err(CompileError {
            message: "Can't parse the attribute's argument",
            span: attr.span(),
        });
    };

    if !meta.path.is_ident("debug") {
        return Err(CompileError {
            message: "The left part of the attribute is not 'debug'",
            span: meta.path.span(),
        });
    }

    if let syn::Expr::Lit(lit_expr) = &meta.value {
        if let syn::Lit::Str(lit_str) = &lit_expr.lit {
            Ok(lit_str.to_owned())
        } else {
            return Err(CompileError {
                message: "The value of the attribute is not a string",
                span: lit_expr.lit.span(),
            });
        }
    } else {
        return Err(CompileError {
            message: "The value of the expression is not a literal",
            span: meta.value.span(),
        });
    }
}

fn create_debug_field_impl(field: &syn::Field) -> proc_macro2::TokenStream {
    let ident_result = field.ident.to_owned().ok_or(CompileError {
        span: field.ident.span(),
        message: "Field has no identifier",
    });

    let ident = match ident_result {
        Ok(value) => value,
        Err(err) => return err.to_token(),
    };

    let ident_str = syn::LitStr::new(&ident.to_string(), field.span());

    let format_str_option = field
        .attrs
        .iter()
        .filter_map(|attr| extract_format_string(attr).ok())
        .last();

    if let Some(format_str) = format_str_option {
        quote! {
          .field(#ident_str, &format_args!(#format_str, &self.#ident))
        }
    } else {
        quote! {
          .field(#ident_str, &self.#ident)
        }
    }
}
