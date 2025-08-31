use std::iter::Peekable;

use proc_macro2::{
    Delimiter, Group, Literal, Punct, Span, TokenStream, TokenTree, token_stream::IntoIter,
};
use syn::{
    Error, Ident, LitInt, Token,
    parse::{Parse, ParseStream},
    parse_macro_input,
    spanned::Spanned,
};

struct Seq {
    start: u32,
    end: u32,
    body: BodyTree,
}

#[derive(Clone)]
enum BodyTree {
    Ident(Ident),
    Punct(Punct),
    Literal(Literal),
    Replace {
        span: Span,
        prefix: Option<String>,
        suffix: Option<String>,
    },
    Group {
        delimiter: Delimiter,
        tree: Vec<BodyTree>,
    },
    Repeating(Vec<BodyTree>),
}

impl BodyTree {
    fn parse(input: TokenStream, replace: &str) -> syn::Result<Self> {
        struct Stack {
            iter: Peekable<IntoIter>,
            tree: Vec<BodyTree>,
            kind: TreeKind,
        }
        enum TreeKind {
            Repeating,
            Group(Delimiter),
        }

        let span = input.span();
        let mut input = input.into_iter();

        let Some(TokenTree::Group(root)) = input.next() else {
            return Err(Error::new(span, "expected a code block!"));
        };
        if let Some(x) = input.next() {
            return Err(Error::new(x.span(), "unexpected tokens after code block!"));
        }

        let mut stack = vec![Stack {
            iter: root.stream().into_iter().peekable(),
            tree: Vec::new(),
            kind: TreeKind::Repeating,
        }];

        loop {
            // Process last element on stack
            while let Some(last) = stack.last_mut()
                && let Some(token_tree) = last.iter.next()
            {
                match token_tree {
                    TokenTree::Ident(ident) if ident == replace => last.tree.push(Self::Replace {
                        span: ident.span(),
                        prefix: None,
                        suffix: None,
                    }),
                    TokenTree::Ident(ident) => last.tree.push(Self::Ident(ident)),

                    TokenTree::Literal(literal) => last.tree.push(Self::Literal(literal)),

                    TokenTree::Group(group) => stack.push(Stack {
                        iter: group.stream().into_iter().peekable(),
                        tree: Vec::new(),
                        kind: TreeKind::Group(group.delimiter()),
                    }),

                    TokenTree::Punct(p) if p.as_char() == '#' => {
                        if let Some(TokenTree::Group(group)) = last.iter.peek()
                            && group.delimiter() == Delimiter::Parenthesis
                        {
                            let iter = group.stream().into_iter().peekable();
                            last.iter.next();
                            last.iter.next(); // HOTFIX: the * at the end, should be hadled

                            if let Some(Stack { kind, .. }) = stack.first_mut() {
                                *kind = TreeKind::Group(Delimiter::Brace);
                            }

                            stack.push(Stack {
                                iter,
                                tree: Vec::new(),
                                kind: TreeKind::Repeating,
                            });
                        } else {
                            last.tree.push(Self::Punct(p));
                        }
                    }

                    TokenTree::Punct(p) if p.as_char() == '~' => {
                        let (Some(prev), Some(TokenTree::Ident(next))) =
                            (last.tree.last_mut(), last.iter.peek())
                        else {
                            last.tree.push(Self::Punct(p));
                            continue;
                        };

                        match (prev, next) {
                            (Self::Ident(prefix), ident) if ident == replace => {
                                *last.tree.last_mut().unwrap() = Self::Replace {
                                    span: prefix.span(),
                                    prefix: Some(prefix.to_string()),
                                    suffix: None,
                                };
                                last.iter.next();
                            }
                            (Self::Replace { suffix, .. }, sfx) if sfx != replace => {
                                suffix.replace(sfx.to_string());
                                last.iter.next();
                            }
                            _ => last.tree.push(Self::Punct(p)),
                        }
                    }

                    TokenTree::Punct(punct) => last.tree.push(Self::Punct(punct)),
                }
            }

            // Last element is processed completely
            let Stack { tree, kind, .. } = stack.pop().unwrap();
            let last = match kind {
                TreeKind::Repeating => Self::Repeating(tree),
                TreeKind::Group(delimiter) => Self::Group { delimiter, tree },
            };

            if let Some(prev) = stack.last_mut() {
                prev.tree.push(last);
            } else {
                return Ok(last);
            }
        }
    }
}

impl Parse for Seq {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let var = input.parse::<Ident>()?;
        input.parse::<Token![in]>()?;

        let start = input.parse::<LitInt>()?.base10_parse()?;
        input.parse::<Token![..]>()?;

        let end = u32::from(input.parse::<Token![=]>().is_ok())
            + input.parse::<LitInt>()?.base10_parse::<u32>()?;

        let body = BodyTree::parse(input.parse()?, &var.to_string())?;

        Ok(Self { start, end, body })
    }
}

#[proc_macro]
// #[allow(unused)]
pub fn seq(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    // eprintln!("{:#?}", proc_macro2::TokenStream::from(input));

    struct Stack {
        iter: std::vec::IntoIter<BodyTree>,
        token_stream: TokenStream,
        kind: TreeKind,
    }

    enum TreeKind {
        Repeating {
            iter: std::vec::IntoIter<BodyTree>,
            start: u32,
            end: u32,
        },
        Group(Delimiter),
    }

    let Seq { start, end, body } = parse_macro_input!(input as Seq);

    let mut stack = vec![match body {
        BodyTree::Group { delimiter, tree } => Stack {
            iter: tree.into_iter(),
            token_stream: TokenStream::new(),
            kind: TreeKind::Group(delimiter),
        },
        BodyTree::Repeating(tree) => Stack {
            iter: tree.clone().into_iter(),
            token_stream: TokenStream::new(),
            kind: TreeKind::Repeating {
                iter: tree.into_iter(),
                start,
                end,
            },
        },
        _ => panic!(),
    }];

    loop {
        // Process last element on stack
        while let Some(body_tree) = stack.last_mut().unwrap().iter.next() {
            match body_tree {
                BodyTree::Replace {
                    span,
                    prefix,
                    suffix,
                } => {
                    // TODO: this shit is so ass
                    let s = stack
                        .iter()
                        .rev()
                        .find_map(|x| {
                            if let TreeKind::Repeating { start, .. } = x.kind {
                                Some(start)
                            } else {
                                None
                            }
                        })
                        .unwrap();

                    let val = if prefix.is_none() && suffix.is_none() {
                        TokenTree::Literal(Literal::u32_unsuffixed(s))
                    } else {
                        TokenTree::Ident(Ident::new(
                            &format!(
                                "{}{s}{}",
                                prefix.unwrap_or_default(),
                                suffix.unwrap_or_default()
                            ),
                            span,
                        ))
                    };
                    stack.last_mut().unwrap().token_stream.extend([val]);
                }

                BodyTree::Group { delimiter, tree } => stack.push(Stack {
                    iter: tree.into_iter(),
                    token_stream: TokenStream::new(),
                    kind: TreeKind::Group(delimiter),
                }),
                BodyTree::Repeating(tree) => stack.push(Stack {
                    iter: tree.clone().into_iter(),
                    token_stream: TokenStream::new(),
                    kind: TreeKind::Repeating {
                        iter: tree.into_iter(),
                        start,
                        end,
                    },
                }),

                // TODO: don't really care about those
                BodyTree::Ident(ident) => stack
                    .last_mut()
                    .unwrap()
                    .token_stream
                    .extend([TokenTree::Ident(ident)]),
                BodyTree::Punct(punct) => stack
                    .last_mut()
                    .unwrap()
                    .token_stream
                    .extend([TokenTree::Punct(punct)]),
                BodyTree::Literal(literal) => {
                    stack
                        .last_mut()
                        .unwrap()
                        .token_stream
                        .extend([TokenTree::Literal(literal)]);
                }
            }
        }

        // Last element is processed completely
        let last = stack.last_mut().unwrap();

        match &mut last.kind {
            TreeKind::Repeating { iter, start, end } => {
                *start += 1;

                if start < end {
                    last.iter = iter.clone();
                    continue;
                } // else start >= end

                let Some(Stack { token_stream, .. }) = stack.pop() else {
                    panic!()
                };

                if let Some(x) = stack.last_mut() {
                    x.token_stream.extend(token_stream);
                } else {
                    return token_stream.into();
                }
            }
            TreeKind::Group(_) => {
                let Some(Stack {
                    token_stream,
                    kind: TreeKind::Group(delimiter),
                    ..
                }) = stack.pop()
                else {
                    panic!()
                };

                if let Some(x) = stack.last_mut() {
                    x.token_stream
                        .extend([TokenTree::Group(Group::new(delimiter, token_stream))]);
                } else {
                    return token_stream.into();
                }
            }
        }
    }
}
