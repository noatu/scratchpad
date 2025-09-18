use proc_macro::TokenStream;
use std::collections::HashSet;

use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::{quote, quote_spanned};
use syn::{
    Data, DeriveInput, Error, Expr, Fields, Generics, Ident, Lit, LitStr, Meta, PathSegment,
    Result, Token, Type, TypePath, WherePredicate,
    parse::{Parse, ParseStream, Parser},
    parse_macro_input, parse_quote_spanned,
    punctuated::Punctuated,
    spanned::Spanned,
    visit::{self, Visit},
};

/// Parse the `Ident = TokenStream` that can be found inside `#[attr(name = value)]`.
struct AttrAssign {
    ident: Ident,
    ts2: TokenStream2,
}
impl Parse for AttrAssign {
    fn parse(input: ParseStream) -> Result<Self> {
        let ident = input.parse()?;
        input.parse::<Token![=]>()?;
        let ts2 = input.parse()?;
        Ok(Self { ident, ts2 })
    }
}

struct AutomaticBounds {
    generics: HashSet<Ident>,
    bounds: HashSet<TypePath>,
}

impl AutomaticBounds {
    fn new(generics: &Generics) -> Self {
        Self {
            generics: generics.type_params().map(|tp| tp.ident.clone()).collect(),
            bounds: HashSet::new(),
        }
    }
}

impl<'ast> Visit<'ast> for AutomaticBounds {
    fn visit_path_segment(&mut self, i: &'ast PathSegment) {
        if i.ident != "PhantomData" {
            visit::visit_path_arguments(self, &i.arguments);
        }
    }

    fn visit_type_path(&mut self, i: &'ast TypePath) {
        for ident in &self.generics {
            if i.path.is_ident(ident) {
                self.bounds.insert(i.clone());
                return;
            }
        }

        let mut iter = i.path.segments.iter();
        if let (Some(seg), Some(_), None) = (iter.next(), iter.next(), iter.next())
            && self.generics.contains(&seg.ident)
        {
            self.bounds.insert(i.clone());
            return;
        }

        visit::visit_type_path(self, i);
    }
}

fn derive_impl(input: DeriveInput) -> Result<TokenStream2> {
    let Data::Struct(data_struct) = input.data else {
        return Err(Error::new(Span::call_site(), "expected a struct"));
    };
    let Fields::Named(fields) = data_struct.fields else {
        return Err(Error::new(Span::call_site(), "expected named fields"));
    };

    let mut generics = input.generics;
    let mut auto_bounds = true;
    let bounds_parser = Punctuated::<WherePredicate, Token![,]>::parse_terminated;

    for attr in input.attrs {
        if !attr.path().is_ident("debug") {
            continue;
        }

        let AttrAssign { ident, ts2 } = attr.parse_args()?;

        if ident != "bound" {
            return Err(Error::new(ident.span(), "unknown attribute"));
        }

        generics
            .make_where_clause()
            .predicates
            .extend(bounds_parser.parse2(ts2)?.into_iter());
        // disable automatic bounds even if bounds are empty
        auto_bounds = false;
    }

    let mut field_types: Vec<Type> = Vec::new();
    let mut field_formats: Vec<(Ident, Option<LitStr>)> = Vec::new();

    for field in fields.named {
        let ident = field
            .ident // NOTE: could unwrap though
            .ok_or_else(|| Error::new(Span::call_site(), "expected named fields"))?;

        let mut field_auto_bounds = true;
        let mut format: Option<LitStr> = None;

        for attr in field.attrs {
            if !attr.path().is_ident("debug") {
                continue;
            }

            match attr.meta {
                Meta::List(_) => {
                    let AttrAssign { ident, ts2 } = attr.parse_args()?;

                    if ident != "bound" {
                        return Err(Error::new(ident.span(), "unknown attribute"));
                    }

                    generics
                        .make_where_clause()
                        .predicates
                        .extend(bounds_parser.parse2(ts2)?.into_iter());
                    // disable automatic bounds even if bounds are empty
                    field_auto_bounds = false;
                }
                Meta::NameValue(nv) => {
                    if let Some(format) = format {
                        let mut err =
                            Error::new(nv.value.span(), "expected only one format attribute");
                        err.combine(Error::new(format.span(), "first format attribute"));
                        return Err(err);
                    }

                    if let Expr::Lit(lit) = &nv.value
                        && let Lit::Str(s) = &lit.lit
                    {
                        format = Some(s.clone());
                    } else {
                        return Err(Error::new(nv.value.span(), "expected a str"));
                    }
                }
                Meta::Path(p) => return Err(Error::new(p.span(), "unknown attribute")),
            }
        }

        if auto_bounds && field_auto_bounds {
            field_types.push(field.ty);
        }
        field_formats.push((ident, format));
    }

    let mut auto_bounds = AutomaticBounds::new(&generics);
    for ty in field_types {
        auto_bounds.visit_type(&ty);
    }
    generics.make_where_clause().predicates.extend(
        auto_bounds.bounds.into_iter().map::<WherePredicate, _>(
            |tp| parse_quote_spanned! {tp.span()=> #tp: ::std::fmt::Debug},
        ),
    );

    let ident = input.ident;
    let ident_str = ident.to_string();
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let impl_fields = field_formats.into_iter().map(|(ident, format)| {
        let ident_str = ident.to_string();
        format.as_ref().map_or_else(
            || quote_spanned!(ident.span()=> .field(#ident_str, &self.#ident)),
            |s| quote_spanned!(s.span()=> .field(#ident_str, &format_args!(#s, &self.#ident))),
        )
    });

    Ok(quote! {
        #[allow(clippy::type_repetition_in_bounds)]
        impl #impl_generics ::std::fmt::Debug for #ident #ty_generics #where_clause {
            fn fmt(&self, fmt: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                fmt.debug_struct(#ident_str)
                    #( #impl_fields )*
                   .finish()
            }
        }
    })
}

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    TokenStream::from(match derive_impl(input) {
        Ok(ts2) => ts2,
        Err(e) => e.into_compile_error(),
    })
}
