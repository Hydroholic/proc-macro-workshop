use proc_macro::TokenStream;
use quote::{quote, quote_spanned};
use syn::{self, spanned::Spanned};
use proc_macro2;


struct CompileError<'a> {
    message: &'a str,
    span: Option<proc_macro2::Span>,
}

impl<'a> CompileError<'a> {
    fn to_token(&self) -> proc_macro2::TokenStream {
        let message = self.message;
        if let Some(span) = self.span {
            quote_spanned! { span => compile_error!(#message) }
        } else { quote! { compile_error!(#message) } }
    }
}

#[derive(Clone)]
struct FieldWithIdent(syn::Field);

impl FieldWithIdent {
    fn extract_ident(&self) -> syn::Ident {
        self.0.ident.to_owned().expect("Field must have an identifier")
    }
}


struct FieldWithRepeatableBuilderName {
    field: FieldWithIdent,
    name: syn::LitStr,
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
    } else { 
        return quote! { compile_error!("Expected struct input") }.into()
    };

    let named_fields: Vec<FieldWithIdent> = if let syn::Fields::Named(f) = fields {
        f.named.into_iter().map(FieldWithIdent).collect()
    } else { 
        return quote! { compile_error!("Expected named fields") }.into()
    };
    
    let res_repeatable_builders = named_fields
        .iter()
        .map(|f| {
            f.0.attrs.iter()
                .filter(|&attr| attr.path().is_ident("builder"))
                .map(get_repetitive_builder_name)
                .map(|res| res.map(|res_ok| FieldWithRepeatableBuilderName {field: f.to_owned(), name: res_ok}))
        })
        .flatten()
        .collect::<Result<Vec<FieldWithRepeatableBuilderName>, CompileError>>();

    let repeatable_builders = match res_repeatable_builders {
        Ok(v) => v,
        Err(e) => return e.to_token().into(),
    };

    let builder_methods_repeated_element = repeatable_builders
        .iter()
        .map(|s| create_builder_method_repeated_element(&s.field, s.name.to_owned()));

    let builder_fields = named_fields.iter().map(create_builder_fields);

    let builder_field_matchings = named_fields.iter().map(create_builder_field_matching);

    let assign_fields = named_fields.iter().map(create_builder_field_assignment);
    
    let builder_methods = named_fields.iter()
        .filter(|&f| {
            find_field_name_in_iterator(
                repeatable_builders.iter()
                    .map(|f| f.name.to_owned()), f)
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

fn get_repetitive_builder_name(attr: &syn::Attribute) -> Result<syn::LitStr, CompileError> {
    let expr = parse_args(attr)
        .or(Err(CompileError {
            message: "Can't parse the attribute's argument",
            span: Some(attr.span()),
        }))?;

    let expr_assign = if let syn::Expr::Assign(e) = expr {
        e
    } else {
        return Err(CompileError {
            message: "The expression is not be an assignment",
            span: Some(expr.span()),
        })
    };

    if let syn::Expr::Path(expr_path) = *expr_assign.left {
        if !expr_path.path.is_ident("each") {
            return Err(CompileError {
                message: "The left part of the expression is not 'each'",
                span: Some(expr_path.path.span()),
            })
        };
    } else {
        return Err(CompileError{ message: "The left part of the expression is not a path", span: Some(expr_assign.left.span()) })
    };

    if let syn::Expr::Lit(lit_expr) = *expr_assign.right {
        if let syn::Lit::Str(lit_str) = lit_expr.lit {
            Ok(lit_str)
        } else {
            return Err(CompileError {
                message: "The right part of the expression is not a string",
                span: Some(lit_expr.lit.span()),
            })
        }
    } else {
        return Err(CompileError {
            message: "The right part of the expression is not a literal",
            span: Some(expr_assign.right.span()),
        })
    }
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
    let field_type = if is_vec_type(field) {
        get_generic_type_argument(field) 
    } else {
        return quote_spanned!{
            field.0.ty.span() => compile_error!("Field must be of Vec type.")
        }
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
