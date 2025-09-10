use proc_macro::TokenStream;

use proc_macro2::Span;
use quote::ToTokens;
use syn::{
    Error, ExprMatch, Item, Pat, Result, parse_macro_input, parse_quote,
    spanned::Spanned,
    visit_mut::{self, VisitMut},
};

#[proc_macro_attribute]
pub fn sorted(_args: TokenStream, input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as Item);

    let Item::Enum(item) = input else {
        return Error::new(Span::call_site(), "expected enum or match expression")
            .into_compile_error()
            .into();
    };

    for i in 0..item.variants.len() - 1 {
        let a = item.variants[i].ident.to_string();
        let b = item.variants[i + 1].ident.to_string();
        let b_span = item.variants[i + 1].ident.span();

        if a <= b {
            continue;
        }

        let c = item
            .variants
            .iter()
            .map(|v| v.ident.to_string())
            .find(|s| s > &b)
            .unwrap_or(a); // a <= b, so a is > b, always
        return Error::new(b_span, format!("{b} should sort before {c}"))
            .to_compile_error()
            .into();
    }

    item.into_token_stream().into()
}

#[proc_macro_attribute]
pub fn check(_args: TokenStream, input: TokenStream) -> TokenStream {
    let mut input = parse_macro_input!(input as Item);

    MatchVisitor.visit_item_mut(&mut input);
    input.into_token_stream().into()
}

struct MatchVisitor;

impl VisitMut for MatchVisitor {
    fn visit_expr_match_mut(&mut self, item: &mut ExprMatch) {
        visit_mut::visit_expr_match_mut(self, item); // for nested matches

        if let Err(e) = check_match_expr(item) {
            let e = e.to_compile_error();
            *item.expr = parse_quote!(#e);
        }
    }
}

fn check_match_expr(item: &mut ExprMatch) -> Result<()> {
    let Some(i) = item.attrs.iter().position(|a| a.path().is_ident("sorted")) else {
        return Ok(());
    };
    item.attrs.remove(i);

    for i in 0..item.arms.len() - 1 {
        if let Pat::Wild(w) = &item.arms[i].pat {
            return Err(Error::new(w.span(), "wildcard must be last"));
        }
        if let Pat::Wild(_) = &item.arms[i + 1].pat {
            continue; // if `_` isn't last, it to be caught next iteration
        }

        let (a, _) = pat_to_str(&item.arms[i].pat)?;
        let (b, b_span) = pat_to_str(&item.arms[i + 1].pat)?;

        if a <= b {
            continue;
        }

        let c = item
            .arms
            .iter()
            .map(|x| {
                pat_to_str(&x.pat)
                    .expect("find() will stop before unchecked elements")
                    .0
            })
            .find(|s| s > &b)
            .unwrap_or(a);
        return Err(Error::new(b_span, format!("{b} should sort before {c}")));
    }

    Ok(())
}

fn pat_to_str(p: &Pat) -> Result<(String, Span)> {
    let path = match p {
        Pat::Path(p) => &p.path,
        Pat::Struct(s) => &s.path,
        Pat::TupleStruct(ts) => &ts.path,
        Pat::Ident(i) => return Ok((i.ident.to_string(), i.span())),
        _ => return Err(Error::new(p.span(), "unsupported by #[sorted]")),
    };
    Ok((
        path.segments
            .iter()
            .map(|s| s.ident.to_string())
            .reduce(|a, s| a + "::" + &s)
            .expect("match pattern cannot be empty"),
        path.span(),
    ))
}
