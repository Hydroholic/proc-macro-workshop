use proc_macro::TokenStream;
use quote::{quote, quote_spanned};
use syn::{self, spanned::Spanned};
use proc_macro2;


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
        .or(
            Err(
                CompileError {
                    message: "Can't parse the attribute's argument",
                    span: attr.span(),
        }))?;

    let expr_assign = if let syn::Expr::Assign(e) = expr {
        e
    } else {
        return Err(
            CompileError {
                message: "The expression is not be an assignment",
                span: expr.span(),
    })};

    if let syn::Expr::Path(expr_path) = *expr_assign.left {
        if !expr_path.path.is_ident("each") {
            return Err(
                CompileError {
                    message: "The left part of the expression is not 'each'",
                    span: expr_path.path.span(),
        })};
    } else {
        return Err(
            CompileError{
                message: "The left part of the expression is not a path",
                span: expr_assign.left.span()
    })};

    if let syn::Expr::Lit(lit_expr) = *expr_assign.right {
        if let syn::Lit::Str(lit_str) = lit_expr.lit {
            Ok(lit_str)
        } else {
            return Err(CompileError {
                message: "The right part of the expression is not a string",
                span: lit_expr.lit.span(),
        })}
    } else {
        return Err(
            CompileError {
                message: "The right part of the expression is not a literal",
                span: expr_assign.right.span(),
    })}
}

fn get_path_field_type(field: &FieldWithIdent) -> Result<&syn::Ident, CompileError> {
    let path = if let syn::Type::Path(p) = &field.0.ty {
        p
    } else { 
        return Err(
            CompileError {
                message: "Field must be a path.",
                span: field.0.ty.span(),
    })};

    let last = path.path.segments.last();

    Ok(
        &last.ok_or(
            CompileError {
                message: "Path must have a last segment.",
                span: last.span(),
        })?.ident
    )
}

fn is_option_type(field: &FieldWithIdent) -> Result<bool, CompileError> {
    Ok(get_path_field_type(field)?.to_string() == "Option".to_string())
}

fn is_vec_type(field: &FieldWithIdent) -> Result<bool, CompileError> {
    Ok(get_path_field_type(field)?.to_string() == "Vec".to_string())
}

fn get_generic_type_argument(field: &FieldWithIdent) -> Result<&syn::Type, CompileError> {
    let path = if let syn::Type::Path(p) = &field.0.ty {
        p
    } else {
        return Err(
            CompileError {
                message: "Field type is not a path",
                span: field.0.span(),
    })};
    
    let last = path.path.segments.last();
    let last_segment = last.ok_or(
        CompileError {
            message: "Path must have a last segment.",
            span: last.span(),
    })?;
    
    let argument = if let syn::PathArguments::AngleBracketed(a) = &last_segment.arguments {
        a.args.first().ok_or(
            CompileError {
                message: "No generic parameter inside the brackets",
                span: a.args.first().span(),
    })?} else {
        return Err(
            CompileError {
                message: "No generic parameter",
                span: last_segment.arguments.span(),
    })};
    
    if let syn::GenericArgument::Type(t) = argument {
        Ok(t)
    } else {
        Err(
            CompileError {
                message: "Generic parameter is not a type",
                span: argument.span()
    })}
}

fn create_builder_field_assignment(field: &FieldWithIdent) -> proc_macro2::TokenStream {
    let identifier = field.extract_ident();
    quote! { #identifier: #identifier.to_owned() }
}

fn create_builder_default_field_assignment(field: &FieldWithIdent) -> proc_macro2::TokenStream {
    let field_name = field.extract_ident();
    match is_vec_type(field) {
        Ok(false) => quote! { #field_name: None },
        Ok(true) => quote! { #field_name: Vec::new() },
        Err(err) => err.to_token()
    }
}

fn create_builder_fields(field: &FieldWithIdent) -> proc_macro2::TokenStream {
    let identifier = field.extract_ident();
    let field_type = field.0.ty.clone();
    let is_option_type = match is_option_type(field) {
        Ok(val) => val,
        Err(err) => return err.to_token()
    };
    let is_vec_type = match is_vec_type(field) {
        Ok(val) => val,
        Err(err) => return err.to_token()
    };
    match is_option_type || is_vec_type {
        true => quote! { #identifier: #field_type },
        false => quote! { #identifier: Option<#field_type> },
    }
}

fn create_builder_field_matching(field: &FieldWithIdent) -> proc_macro2::TokenStream {
    let identifier = field.extract_ident();
    let is_option_type = match is_option_type(field) {
        Ok(val) => val,
        Err(err) => return err.to_token()
    };
    let is_vec_type = match is_vec_type(field) {
        Ok(val) => val,
        Err(err) => return err.to_token()
    };
    match is_option_type || is_vec_type  {
        true => quote! { #identifier },
        false => quote! { #identifier: Some(#identifier) },
    }
}

fn create_builder_method(field: &FieldWithIdent) -> proc_macro2::TokenStream {
    let is_type_with_generic = is_option_type(field);
    let field_type = match is_type_with_generic {
        Ok(false) => &field.0.ty,
        Ok(true) => match get_generic_type_argument(field) {
            Ok(generic) => generic,
            Err(err) => return err.to_token(),
        },
        Err(err) => return err.to_token(),
    };
    let is_vec_type = match is_vec_type(field) {
        Ok(val) => val,
        Err(err) => return err.to_token()
    };
    let field_value = if is_vec_type {
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
    let is_vec_type = match is_vec_type(field) {
        Ok(val) => val,
        Err(err) => return err.to_token()
    };
    let field_type = if is_vec_type {
        match get_generic_type_argument(field) {
            Ok(value) => value,
            Err(err) => return err.to_token(),
        }
    } else {
        return CompileError {
            span: field.0.ty.span(),
            message: "Field must be of Vec type.",
        }.to_token()
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
