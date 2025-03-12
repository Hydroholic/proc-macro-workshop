use proc_macro::{TokenStream, Span};
use quote::quote;
use syn;
use proc_macro2;

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = syn::parse_macro_input!(input as syn::DeriveInput);

    let name = ast.ident;
    let name_builder_str = format!("{}Builder", name);
    let name_builder = syn::Ident::new(
        &name_builder_str,
        Span::call_site().into()
    );

    let fields = match ast.data {
        syn::Data::Struct(data) => Some(data),
        _ => None,
    }.expect("Expected struct input").fields;

    let named_fields = match fields {
        syn::Fields::Named(named_fields) => Some(named_fields),
        _ => None,
    }.expect("Expected named fields").named;

    // let field = named_fields.iter().find(|&field| {
    //     field.ident.clone().is_some_and(|ident| ident.to_string() == "current_dir".to_string())
    // }).expect("No field named current_dir");

    let builder_fields = named_fields
        .iter()
        .map(
            |field| {
                let identifier = field.ident.clone().expect("Field must have an identifier");
                if is_option_type(field) {
                    quote! { #identifier }
                } else {
                    quote! { #identifier: Some(#identifier) }
                }
            }
        );

    let assign_fields = named_fields
        .iter()
        .map(
            |field| {
                let identifier = field.ident.clone().expect("Field must have an identifier");
                quote! { #identifier: #identifier.to_owned() }
            }
        );
    
    let builder_methods = named_fields
        .iter()
        .map(create_builder_method);

    quote! {
        use std::error::Error;

        pub struct #name_builder {
            executable: Option<String>,
            args: Option<Vec<String>>,
            env: Option<Vec<String>>,
            current_dir: Option<String>,
        }

        impl #name_builder {
            #(#builder_methods)*

            pub fn build(&mut self) -> Result<#name, Box<dyn Error>> {
                if let #name_builder {
                    #(#builder_fields),*
                } = self {
                    Ok(
                        #name {
                            #(#assign_fields),*
                        }
                    )
                } else {
                    Err(Box::from("Could not build because of missing attributes."))
                }
            }
        }

        impl #name {
            pub fn builder() -> #name_builder {
                #name_builder {
                    executable: None,
                    args: None,
                    env: None,
                    current_dir: None,
                }
            }
        }
    }.into()
}

fn is_option_type(field: &syn::Field) -> bool {
    let path = if let syn::Type::Path(p) = &field.ty {
        p
    } else { return false; };

    let last_segment = if let Some(ps) = path.path.segments.last() {
        ps
    } else { return false; };
    
    if last_segment.ident.to_string() == "Option".to_string() {
        true
    } else {
        false
    }
}

fn create_builder_method(field: &syn::Field) -> proc_macro2::TokenStream {
    let field_type = match is_option_type(field) {
        false => &field.ty,
        true => {
            let path = if let syn::Type::Path(p) = &field.ty {
                p
            } else { panic!("type not a path"); };
        
            let last_segment = if let Some(ps) = path.path.segments.last() {
                ps
            } else { panic!("no last segment in path") };

            let argument = if let syn::PathArguments::AngleBracketed(a) = &last_segment.arguments {
                a.args.first().expect("Option should have one generic argument")
            } else {
                panic!("{}", 
                    match &last_segment.arguments {
                        syn::PathArguments::None => "None",
                        syn::PathArguments::Parenthesized(_) => "parenthesized",
                        _ => "wtf"
                    }
                )
            };

            if let syn::GenericArgument::Type(t) = argument {
                t
            } else {
                panic!("generic argument not a type")
            }
        }
    };
    let field_name = field.ident.to_owned().expect("Field must have an identifier");
    quote! {
        fn #field_name(&mut self, value: #field_type) -> &mut Self {
            self.#field_name = Some(value);
            self
        }
    }
}