use proc_macro::TokenStream;
use std::collections::HashSet;

use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::quote;
use syn::{
    Data, DeriveInput, Error, Fields, Generics, Ident, Meta, PathSegment, Result, Token, TypePath,
    WherePredicate,
    parse::{Parse, ParseStream, Parser},
    parse_macro_input, parse_quote_spanned,
    punctuated::Punctuated,
    spanned::Spanned,
    visit::{self, Visit},
};

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
    generics: Vec<Ident>,
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

// struct AttrBounds(Punctuated<WherePredicate, Token![,]>);

// impl Parse for AttrBounds {
//     fn parse(input: ParseStream) -> Result<Self> {
//         let bound: Ident = input.parse()?;
//         if bound != "bound" {
//             return Err(Error::new(bound.span(), "unrecognized attribute"));
//         }
//         input.parse::<Token![=]>()?;
//         Ok(Self(Punctuated::parse_separated_nonempty(input)?))
//     }
// }

fn derive_impl(input: DeriveInput) -> Result<TokenStream2> {
    let Data::Struct(data_struct) = &input.data else {
        return Err(Error::new(Span::call_site(), "expected a struct"));
    };
    let Fields::Named(fields) = &data_struct.fields else {
        return Err(Error::new(Span::call_site(), "expected named fields"));
    };
    let fields = &fields.named;

    let mut bounds = Vec::new();

    for attr in &input.attrs {
        if !attr.path().is_ident("debug") {
            continue;
        }

        let AttrAssign { ident, ts2 } = attr.parse_args()?;

        if ident == "bound" {
            let parser = Punctuated::<WherePredicate, Token![,]>::parse_separated_nonempty;
            bounds.extend(parser.parse2(ts2)?.into_iter());
        } else {
            return Err(Error::new(ident.span(), "unknown attribute"));
        }
    }

    let auto_inference = bounds.is_empty();

    let mut impl_fields = Vec::new();
    let mut auto_bounds = AutomaticBounds::new(&input.generics);

    for field in fields {
        let mut field_format = Vec::new();
        let mut field_bounds = Vec::new();

        for attr in &field.attrs {
            if !attr.path().is_ident("debug") {
                continue;
            }

            match &attr.meta {
                Meta::NameValue(nv) => field_format.push(&nv.value),
                Meta::List(_) => {
                    let AttrAssign { ident, ts2 } = attr.parse_args()?;
                    if ident == "bound" {
                        let parser =
                            Punctuated::<WherePredicate, Token![,]>::parse_separated_nonempty;
                        field_bounds.extend(parser.parse2(ts2)?.into_iter());
                    } else {
                        return Err(Error::new(ident.span(), "unknown attribute"));
                    }
                }
                Meta::Path(p) => return Err(Error::new(p.span(), "unknown attribute")),
            }
        }

        let ident = field.ident.as_ref().unwrap();
        let ident_str = ident.to_string();

        if field_format.is_empty() {
            impl_fields.push(quote!(.field(#ident_str, &self.#ident)));
        } else {
            impl_fields.extend(
                field_format
                    .into_iter()
                    .map(|expr| quote!(.field(#ident_str, &format_args!(#expr, &self.#ident)))),
            );
        }

        if auto_inference && field_bounds.is_empty() {
            auto_bounds.visit_type(&field.ty);
        }
        bounds.extend(field_bounds);
    }

    bounds.extend(
        auto_bounds
            .bounds
            .into_iter()
            .map(|tp| parse_quote_spanned! {tp.span()=> #tp: ::std::fmt::Debug}),
    );

    let ident = input.ident;
    let ident_str = ident.to_string();

    let mut generics = input.generics;
    generics.make_where_clause().predicates.extend(bounds);
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    Ok(quote! {
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
    // dbg!(input);
    TokenStream::from(match derive_impl(input) {
        Ok(ts2) => ts2,
        Err(e) => e.into_compile_error(),
    })

    // let ident_str = ident.to_string();
    // for param in &mut generics.params {
    //     if let GenericParam::Type(type_param) = param
    //         && !inside_phantom.contains(&type_param.ident.to_string())
    //     {
    //         type_param.bounds.push(parse_quote!(::std::fmt::Debug));
    //     }
    // }
}
