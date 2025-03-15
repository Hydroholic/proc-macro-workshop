use proc_macro::TokenStream;
use quote::quote;
use syn;
use proc_macro2;


struct FieldWithIdent(syn::Field);

impl FieldWithIdent {
    fn extract_ident(&self) -> syn::Ident {
        self.0.ident.to_owned().expect("Field must have an identifier")
    }
}


#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = syn::parse_macro_input!(input as syn::DeriveInput);

    let name = ast.ident;
    let name_builder_str = format!("{}Builder", name);
    let name_builder = syn::Ident::new(
        &name_builder_str,
        name.span()
    );

    let fields = if let syn::Data::Struct(data) = ast.data {
        data.fields
    } else { panic!("Expected struct input") };

    let named_fields: Vec<FieldWithIdent> = if let syn::Fields::Named(f) = fields {
        f.named.into_iter().map(FieldWithIdent).collect()
    } else { panic!("Expected named fields") };


    let repetitive_builder_names = named_fields
        .iter()
        .map(|f| {
            f.0.attrs.iter()
                .filter(|&attr| attr.path().is_ident("builder"))
                .map(get_repetitive_builder_name)
        });

    let builder_methods_repeated_element = named_fields
        .iter()
        .map(|f| {
            f.0.attrs.iter()
                .filter(|&attr| attr.path().is_ident("builder"))
                .map(get_repetitive_builder_name)
                .map(|s| create_builder_method_repeated_element(f, s))
        }).flatten();

    let builder_fields = named_fields.iter().map(create_builder_fields);

    let builder_field_matchings = named_fields.iter().map(create_builder_field_matching);

    let assign_fields = named_fields.iter().map(create_builder_field_assignment);
    
    let builder_methods = named_fields.iter()
        .filter(|&f| {
            find_field_name_in_iterator(repetitive_builder_names.clone().flatten(), f)
        })
        .map(create_builder_method);

    let assign_field_defaults = named_fields.iter().map(create_builder_default_field_assignment);
    
    quote! {
        use std::error::Error;

        pub struct #name_builder {
            #(#builder_fields),*
        }

        impl #name_builder {
            #(#builder_methods)*

            #(#builder_methods_repeated_element)*

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

fn find_field_name_in_iterator(
    repetitive_builder_names: impl std::iter::Iterator<Item = syn::LitStr>,
    f: &FieldWithIdent
) -> bool {
    repetitive_builder_names
        .map(|literal| literal.value())
        .all(|s| s != f.extract_ident().to_string())
}

fn parse_args(attr: &syn::Attribute) -> Result<syn::Expr, syn::Error> {
    attr.parse_args()
}

fn get_repetitive_builder_name(attr: &syn::Attribute) -> syn::LitStr {
    let expr = parse_args(attr).expect("Can't parse the attribute's argument");

    let expr_assign = if let syn::Expr::Assign(e) = expr {
        e
    } else { panic!("The expression is not be an assignment") };

    if let syn::Expr::Path(expr_path) = *expr_assign.left {
        if !expr_path.path.is_ident("each") {
            panic!("The left part of the expression is not 'each'") 
        };
    } else { panic!("The left part of the expression is not a path") };

    if let syn::Expr::Lit(lit_expr) = *expr_assign.right {
        if let syn::Lit::Str(lit_str) = lit_expr.lit {
            lit_str
        } else { panic!("The right part of the expression is not a string") }
    } else { panic!("The right part of the expression is not a literal") }
}

fn get_path_field_type(field: &FieldWithIdent) -> &syn::Ident {
    let path = if let syn::Type::Path(p) = &field.0.ty {
        p
    } else { panic!("Field must be a path.") };

    &path.path.segments.last().expect("Path must have a last segment.").ident
}

fn is_option_type(field: &FieldWithIdent) -> bool {
    get_path_field_type(field).to_string() == "Option".to_string()
}

fn is_vec_type(field: &FieldWithIdent) -> bool {
    get_path_field_type(field).to_string() == "Vec".to_string()
}

fn get_generic_type_argument(field: &FieldWithIdent) -> &syn::Type {
    let path = if let syn::Type::Path(p) = &field.0.ty {
        p
    } else { panic!("Field type is not a path") };
    
    let last_segment = path.path.segments.last().expect("Path must have a last segment.");
    
    let argument = if let syn::PathArguments::AngleBracketed(a) = &last_segment.arguments {
        a.args.first().expect("No generic parameter inside the brackets")
    } else { panic!("No generic parameter") };
    
    if let syn::GenericArgument::Type(t) = argument {
        t
    } else { panic!("Generic parameter is not a type") }
}

fn create_builder_field_assignment(field: &FieldWithIdent) -> proc_macro2::TokenStream {
    let identifier = field.extract_ident();
    quote! { #identifier: #identifier.to_owned() }
}

fn create_builder_default_field_assignment(field: &FieldWithIdent) -> proc_macro2::TokenStream {
    let field_name = field.extract_ident();
    match is_vec_type(field) {
        false => quote! { #field_name: None },
        true => quote! { #field_name: Vec::new() },
    }
}

fn create_builder_fields(field: &FieldWithIdent) -> proc_macro2::TokenStream {
    let identifier = field.extract_ident();
    let field_type = field.0.ty.clone();
    match is_option_type(field) || is_vec_type(field) {
        true => quote! { #identifier: #field_type },
        false => quote! { #identifier: Option<#field_type> },
    }
}

fn create_builder_field_matching(field: &FieldWithIdent) -> proc_macro2::TokenStream {
    let identifier = field.extract_ident();
    match is_option_type(field) || is_vec_type(field)  {
        true => quote! { #identifier },
        false => quote! { #identifier: Some(#identifier) },
    }
}

fn create_builder_method(field: &FieldWithIdent) -> proc_macro2::TokenStream {
    let is_type_with_generic = is_option_type(field);
    let field_type = match is_type_with_generic {
        false => &field.0.ty,
        true => get_generic_type_argument(field),
    };
    let field_value = if is_vec_type(field) {
        quote! { value }
    } else {
        quote! { Some(value) }
    };

    let field_name = field.extract_ident();
    quote! {
        fn #field_name(&mut self, value: #field_type) -> &mut Self {
            self.#field_name = #field_value;
            self
        }
    }
}

fn create_builder_method_repeated_element(field: &FieldWithIdent, name: syn::LitStr) -> proc_macro2::TokenStream {
    let field_type = match is_vec_type(field) {
        false => panic!("Field must be of Vec type."),
        true => get_generic_type_argument(field),
    };
    let field_name = field.extract_ident();
    let func_name = syn::Ident::new(&name.value(), name.span());
    quote! {
        fn #func_name(&mut self, value: #field_type) -> &mut Self {
            self.#field_name.push(value);
            self
        }
    }
}

// TODO: Group building block by field to avoid decoupled logic on field type
// BUT: Can't because pieces of code like attributes of a struct must be pieced together
