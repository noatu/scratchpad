use std::iter::Peekable;

use proc_macro2::{
    Delimiter, Group, Literal, Span, TokenStream, TokenTree, token_stream::IntoIter,
};
use quote::ToTokens;
use syn::{Ident, LitInt, Token, braced, parse::Parse, parse_macro_input};

struct Seq {
    var: Ident,
    start: u32,
    end: u32,
    body: TokenStream,
}

impl Parse for Seq {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let var = input.parse()?;
        input.parse::<Token![in]>()?;
        let start = input.parse::<LitInt>()?.base10_parse()?;
        input.parse::<Token![..]>()?;
        let end = input.parse::<LitInt>()?.base10_parse()?;
        let content;
        braced!(content in input);

        Ok(Self {
            var,
            start,
            end,
            body: content.parse()?,
        })
    }
}

fn rebuild(ts: TokenStream, replace: &str, val: u32) -> TokenStream {
    struct Stack {
        iter: Peekable<IntoIter>,
        delimeter: Delimiter,
        token_stream: TokenStream,
    }

    let mut stack: Vec<Stack> = vec![Stack {
        iter: ts.into_iter().peekable(),
        delimeter: Delimiter::None,
        token_stream: TokenStream::new(),
    }];

    loop {
        // Process last element on stack
        while let Some(last) = stack.last_mut()
            && let Some(token_tree) = last.iter.next()
        {
            match (token_tree, last.iter.peek()) {
                (TokenTree::Ident(i), _) if i == replace => last
                    .token_stream
                    .extend(Literal::u32_unsuffixed(val).into_token_stream()),

                // Ident~Replace handling
                (TokenTree::Ident(ident), Some(TokenTree::Punct(p))) if p.as_char() == '~' => {
                    let punct = last.iter.next().unwrap();
                    if let Some(TokenTree::Ident(i)) = last.iter.peek()
                        && i == replace
                    {
                        last.iter.next().unwrap();
                        last.token_stream.extend(
                            Ident::new(&format!("{ident}{val}"), Span::call_site())
                                .into_token_stream(),
                        );
                    } else {
                        last.token_stream.extend([TokenTree::Ident(ident), punct]);
                    }
                }

                // Switch processing to the encountered Group
                (TokenTree::Group(g), _) => stack.push(Stack {
                    iter: g.stream().into_iter().peekable(),
                    delimeter: g.delimiter(),
                    token_stream: TokenStream::new(),
                }),
                // TokenTree::Punct(punct) => todo!(),
                (tt, _) => last.token_stream.extend([tt]),
            }
        }

        // Last element is processed completely

        if stack.len() == 1 {
            return stack.pop().unwrap().token_stream;
        }

        let group = stack.pop().expect("stack must have 2 or more elements");
        stack
            .last_mut()
            .expect("stack must have 1 or more element")
            .token_stream
            .extend(Group::new(group.delimeter, group.token_stream).into_token_stream());
    }
}

#[proc_macro]
// #[allow(unused)]
pub fn seq(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let Seq {
        var,
        start,
        end,
        body,
    } = parse_macro_input!(input as Seq);

    // eprintln!("{body:#?}");

    (start..end)
        .map(|x| rebuild(body.clone(), &var.to_string(), x))
        .collect::<TokenStream>()
        .into()
    // Default::default()
}
