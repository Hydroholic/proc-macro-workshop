use proc_macro::{TokenStream, Span};
use quote::quote;
use syn;

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

    let field = named_fields.iter().find(|&field| {
        field.ident.clone().is_some_and(|ident| ident.to_string() == "current_dir".to_string())
    }).expect("No field named current_dir");

    let field_is_option = if let syn::Type::Path(path) = &field.ty {
        path.path.segments
        .last()
        .expect("No segment found in path")
        .ident.to_string() == "Option".to_string()
    } else { false };

    let current_dir_match = if field_is_option {
        quote! { current_dir }
    } else {
        quote! { current_dir: Some(current_dir) }
    };

    quote! {
        use std::error::Error;

        pub struct #name_builder {
            executable: Option<String>,
            args: Option<Vec<String>>,
            env: Option<Vec<String>>,
            current_dir: Option<String>,
        }

        impl #name_builder {
            fn executable(&mut self, value: String) -> &mut Self {
                self.executable = Some(value);
                self
            }

            fn args(&mut self, value: Vec<String>) -> &mut Self {
                self.args = Some(value);
                self
            }

            fn env(&mut self, value: Vec<String>) -> &mut Self {
                self.env = Some(value);
                self
            }

            fn current_dir(&mut self, value: String) -> &mut Self {
                self.current_dir = Some(value);
                self
            }

            pub fn build(&mut self) -> Result<#name, Box<dyn Error>> {
                if let #name_builder {
                    executable: Some(executable),
                    args: Some(args),
                    env: Some(env),
                    #current_dir_match,
                } = self {
                    Ok(
                        #name {
                            executable: executable.to_owned(),
                            args: args.to_owned(),
                            env: env.to_owned(),
                            current_dir: current_dir.to_owned(),
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
