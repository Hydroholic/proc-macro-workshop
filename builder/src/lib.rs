use proc_macro::{TokenStream, Span};
use quote::quote;
use syn;
use proc_macro2;


struct FieldWithIdent(syn::Field);

impl FieldWithIdent {
    fn extract_ident(&self) -> syn::Ident {
        self.0.ident.to_owned().expect("Field must have an identifier")
    }
}


#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = syn::parse_macro_input!(input as syn::DeriveInput);

    let name = ast.ident;
    let name_builder_str = format!("{}Builder", name);
    let name_builder = syn::Ident::new(
        &name_builder_str,
        Span::call_site().into()
    );

    let fields = if let syn::Data::Struct(data) = ast.data {
        data.fields
    } else { panic!("Expected struct input") };

    let named_fields: Vec<FieldWithIdent> = if let syn::Fields::Named(f) = fields {
        f.named.into_iter().map(FieldWithIdent).collect()
    } else { panic!("Expected named fields") };

    let builder_fields = named_fields.iter().map(create_builder_fields);

    let builder_field_matchings = named_fields.iter().map(create_builder_field_matching);

    let assign_fields = named_fields.iter().map(create_builder_field_assignment);
    
    let builder_methods = named_fields.iter().map(create_builder_method);

    let assign_field_defaults = named_fields.iter().map(create_builder_default_field_assignment);
    
    quote! {
        use std::error::Error;

        pub struct #name_builder {
            #(#builder_fields),*
        }

        impl #name_builder {
            #(#builder_methods)*

            pub fn build(&mut self) -> Result<#name, Box<dyn Error>> {
                if let #name_builder {
                    #(#builder_field_matchings),*
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
                    #(#assign_field_defaults),*
                }
            }
        }
    }.into()
}


fn is_option_type(field: &FieldWithIdent) -> bool {
    let path = if let syn::Type::Path(p) = &field.0.ty {
        p
    } else { return false };
    
    let last_segment = if let Some(ps) = path.path.segments.last() {
        ps
    } else { return false };
    
    if last_segment.ident.to_string() == "Option".to_string() {
        true
    } else {
        false
    }
}

fn get_option_inner_type(field: &FieldWithIdent) -> Option<&syn::Type> {
    let path = if let syn::Type::Path(p) = &field.0.ty {
        p
    } else { return None };
    
    let last_segment = path.path.segments.last()?;
    
    let argument = if let syn::PathArguments::AngleBracketed(a) = &last_segment.arguments {
        a.args.first()?
    } else { return None };
    
    if let syn::GenericArgument::Type(t) = argument {
        Some(t)
    } else { None }
}

fn create_builder_field_assignment(field: &FieldWithIdent) -> proc_macro2::TokenStream {
    let identifier = field.extract_ident();
    quote! { #identifier: #identifier.to_owned() }
}

fn create_builder_default_field_assignment(field: &FieldWithIdent) -> proc_macro2::TokenStream {
    let identifier = field.extract_ident();
    quote! { #identifier: None }
}

fn create_builder_fields(field: &FieldWithIdent) -> proc_macro2::TokenStream {
    let identifier = field.extract_ident();
    let field_type = field.0.ty.clone();
    match is_option_type(field) {
        true => quote! { #identifier: #field_type },
        false => quote! { #identifier: Option<#field_type> },
    }
}

fn create_builder_field_matching(field: &FieldWithIdent) -> proc_macro2::TokenStream {
    let identifier = field.extract_ident();
    match is_option_type(field) {
        true => quote! { #identifier },
        false => quote! { #identifier: Some(#identifier) },
    }
}

fn create_builder_method(field: &FieldWithIdent) -> proc_macro2::TokenStream {
    let field_type = match is_option_type(field) {
        false => &field.0.ty,
        true => get_option_inner_type(field).expect("No type found in option."),
    };
    let field_name = field.extract_ident();
    quote! {
        fn #field_name(&mut self, value: #field_type) -> &mut Self {
            self.#field_name = Some(value);
            self
        }
    }
}
