use proc_macro::TokenStream;
use syn::{self, spanned::Spanned};
use quote::{quote, quote_spanned};


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
        return quote! { compile_error!("Expected struct input") }.into()
    };

    let named_fields: Vec<syn::Field> = if let syn::Fields::Named(f) = fields {
        f.named.into_iter().collect()
    } else {
        return quote! { compile_error!("Expected named fields") }.into()
    };

    let debug_fields = named_fields.iter().map(create_debug_field_impl);

    quote! {
        use std::fmt;
        
        impl fmt::Debug for #name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                f.debug_struct(#name_str)
                 #(#debug_fields)*
                 .finish()
            }
        }
    }.into()
}


fn extract_format_string(attr: &syn::Attribute) -> Result<syn::LitStr, CompileError> {
    let meta = if let syn::Meta::NameValue(m) = &attr.meta {
        m
    } else {
        return Err(
            CompileError {
                message: "Can't parse the attribute's argument",
                span: attr.span(),
    })};

    if !meta.path.is_ident("debug") {
        return Err(
            CompileError {
                message: "The left part of the attribute is not 'debug'",
                span: meta.path.span(),
    })}

    if let syn::Expr::Lit(lit_expr) = &meta.value {
        if let syn::Lit::Str(lit_str) = &lit_expr.lit {
            Ok(lit_str.to_owned())
        } else {
            return Err(CompileError {
                message: "The value of the attribute is not a string",
                span: lit_expr.lit.span(),
        })}
    } else {
        return Err(
            CompileError {
                message: "The value of the expression is not a literal",
                span: meta.value.span(),
    })}
}


fn create_debug_field_impl(field: &syn::Field) -> proc_macro2::TokenStream {
    let ident_result = field.ident
        .to_owned()
        .ok_or(
            CompileError { 
                span: field.ident.span(),
                message: "Field has no identifier"});
    
    let ident = match ident_result {
        Ok(value) => value,
        Err(err) => return err.to_token(),
    };

    let ident_str = syn::LitStr::new(&ident.to_string(), field.span());

    let a = field.attrs.iter().map(|attr| extract_format_string(attr));
    for item in a {
        if let Err(err) = item {
            println!("{}", err.message);
            return err.to_token();
        }
    }

    let format_str_option = field.attrs.iter().filter_map(|attr| extract_format_string(attr).ok()).last();

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