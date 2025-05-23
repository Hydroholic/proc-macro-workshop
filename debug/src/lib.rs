use proc_macro::TokenStream;
use quote::{quote, quote_spanned};
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

    let (impl_generics, ty_generics, _) = input.generics.split_for_impl();

    let debug_fields = named_fields.iter().map(create_debug_field_impl);

    let x: Result<Vec<proc_macro2::TokenStream>, CompileError> = input
        .attrs
        .iter()
        .filter(|&x| x.path().is_ident("debug"))
        .map(get_bound_literal)
        .map(|x| {
            x.and_then(|y| {
                let s: syn::WherePredicate = syn::parse_str(&y.value()).or(Err(CompileError {
                    message: "Could not parse WherePredicate",
                    span: y.span(),
                }))?;
                Ok(quote_spanned! { y.span() => where #s})
            })
        })
        .collect();

    let bound_from_attr = match x {
        Ok(v) => v.last().map(|x| x.to_owned()),
        Err(e) => return e.to_token().into(),
    };

    let generics = get_generic_idents(&input.generics, excluded_generics);

    let generics_from_attributes: Vec<syn::TypePath> = named_fields
        .iter()
        .filter_map(|f| get_type_path(&f.ty))
        .filter_map(|f| get_generics_from_attribute(f, &generics))
        .collect();

    let new_where_clause = if bound_from_attr.is_some() {
        bound_from_attr
    } else if generics_from_attributes.is_empty() {
        None
    } else {
        Some(quote! { where
        #(#generics_from_attributes: std::fmt::Debug,)*
        })
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

fn get_inner_type(type_path: &syn::TypePath) -> Option<syn::TypePath> {
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
        Some(generic_type_path.to_owned())
    } else {
        None
    }
}

fn extract_phantom_data_generic(f: &syn::Field) -> Option<&syn::TypePath> {
    let type_path = if let syn::Type::Path(pt) = &f.ty {
        pt
    } else {
        return None;
    };

    let type_ident = &type_path
        .path
        .segments
        .last()
        .expect("Path should have a last segmenbt")
        .ident;

    if type_ident != "PhantomData" {
        return None;
    };

    let arguments = &type_path
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

fn get_generic_idents(generics: &syn::Generics, exclude: Vec<&syn::TypePath>) -> Vec<syn::Ident> {
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

fn get_generics_from_attribute(
    type_path: &syn::TypePath,
    generic_idents: &Vec<syn::Ident>,
) -> Option<syn::TypePath> {
    let a: String = type_path
        .path
        .segments
        .iter()
        .map(|x| x.ident.to_string())
        .reduce(|acc, s| format!("{acc}::{s}"))?;

    let a_str = a.to_string();

    if generic_idents
        .iter()
        .map(|x| x.to_string())
        .any(|x| a_str == x || a_str.starts_with(&format!("{x}::")))
    {
        return Some(type_path.to_owned());
    }

    let inner_type = get_inner_type(type_path)?;

    get_generics_from_attribute(&inner_type, generic_idents)
}

fn get_type_path(t: &syn::Type) -> Option<&syn::TypePath> {
    if let syn::Type::Path(p) = t {
        Some(p)
    } else {
        None
    }
}

fn parse_args(attr: &syn::Attribute) -> Result<syn::Expr, syn::Error> {
    attr.parse_args()
}

fn get_bound_literal(attr: &syn::Attribute) -> Result<syn::LitStr, CompileError> {
    let expr = parse_args(attr).or(Err(CompileError {
        message: "Can't parse the attribute's argument",
        span: attr.span(),
    }))?;

    let expr_assign = if let syn::Expr::Assign(e) = expr {
        e
    } else {
        return Err(CompileError {
            message: "The expression is not be an assignment",
            span: expr.span(),
        });
    };

    if let syn::Expr::Path(expr_path) = *expr_assign.left {
        if !expr_path.path.is_ident("bound") {
            return Err(CompileError {
                message: "The left part of the expression is not 'bound'",
                span: expr_path.path.span(),
            });
        };
    } else {
        return Err(CompileError {
            message: "The left part of the expression is not a path",
            span: expr_assign.left.span(),
        });
    };

    if let syn::Expr::Lit(lit_expr) = *expr_assign.right {
        if let syn::Lit::Str(lit_str) = lit_expr.lit {
            Ok(lit_str)
        } else {
            return Err(CompileError {
                message: "The right part of the expression is not a string",
                span: lit_expr.lit.span(),
            });
        }
    } else {
        return Err(CompileError {
            message: "The right part of the expression is not a literal",
            span: expr_assign.right.span(),
        });
    }
}
