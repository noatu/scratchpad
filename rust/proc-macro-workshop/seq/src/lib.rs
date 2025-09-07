use proc_macro::{Delimiter, Group, Ident, Literal, Punct, Spacing, Span, TokenStream, TokenTree};

struct SeqIR {
    start: usize,
    end: usize,
    tree: Vec<SeqTree>,
}

enum SeqTree {
    Group(GroupTree),
    Repeat(RepeatTree),
    Replace(ReplaceToken),
    Other(TokenTree),
}
struct GroupTree {
    span: Span,
    delimiter: Delimiter,
    tree: Vec<SeqTree>,
}
struct RepeatTree {
    separator: Vec<TokenTree>,
    tree: Vec<SeqTree>,
}
struct ReplaceToken {
    span: Span,
    prefix: Option<String>,
    suffix: Option<String>,
}

impl TryFrom<TokenStream> for SeqIR {
    type Error = TokenStream;

    fn try_from(value: TokenStream) -> Result<Self, Self::Error> {
        let mut iter = value.into_iter();
        let mut last_span = Span::call_site();

        let next = iter.next();
        let var = if let Some(TokenTree::Ident(ident)) = &next {
            last_span = ident.span();
            ident.to_string()
        } else {
            return Err(error(
                "Expected an identifier!",
                next.map_or(last_span, |t| t.span()),
            ));
        };

        let next = iter.next();
        if let Some(TokenTree::Ident(ident)) = &next
            && ident.to_string() == "in"
        {
            last_span = ident.span();
        } else {
            return Err(error(
                "Expected the `in` keyword!",
                next.map_or_else(|| last_span.end(), |t| t.span()),
            ));
        }

        let next = iter.next();
        let start = if let Some(TokenTree::Literal(lit)) = &next
            && let Ok(x) = lit.to_string().parse::<usize>()
        {
            last_span = lit.span();
            x
        } else {
            return Err(error(
                "Expected a usize (range start)!",
                next.map_or_else(|| last_span.end(), |t| t.span()),
            ));
        };

        for _ in 0..2 {
            let next = iter.next();
            if let Some(TokenTree::Punct(punct)) = &next
                && punct.as_char() == '.'
            {
                last_span = punct.span();
            } else {
                return Err(error(
                    "Expected `..`!",
                    next.map_or_else(|| last_span.end(), |t| t.span()),
                ));
            }
        }

        let mut next = iter.next();
        let mut end = 0;

        if let Some(TokenTree::Punct(punct)) = &next
            && punct.as_char() == '='
        {
            last_span = punct.span();
            next = iter.next();
            end += 1;
        }

        if let Some(TokenTree::Literal(lit)) = &next
            && let Ok(x) = lit.to_string().parse::<usize>()
        {
            last_span = lit.span();
            end += x;
        } else {
            return Err(error(
                &format!("Expected a usize, got {next:#?}!"), // FIXME
                next.map_or_else(|| last_span.end(), |t| t.span()),
            ));
        }

        let next = iter.next();

        let block = if let Some(TokenTree::Group(group)) = &next
            && group.delimiter() == Delimiter::Brace
        {
            group.stream()
        } else {
            return Err(error(
                "Expected a code block!",
                next.map_or_else(|| last_span.end(), |t| t.span()),
            ));
        };

        let other: TokenStream = iter.map(|t| error("Unexpected token!", t.span())).collect();
        if !other.is_empty() {
            return Err(other);
        }

        Ok(Self {
            start,
            end,
            tree: SeqTree::parse(block, &var),
        })
    }
}

impl SeqTree {
    fn parse(token_stream: TokenStream, replace: &str) -> Vec<Self> {
        let mut iter = token_stream.into_iter().peekable();
        let mut tree = Vec::new();

        while let Some(token) = iter.next() {
            match token {
                TokenTree::Ident(ident) if ident.to_string() == replace => {
                    tree.push(Self::Replace(ReplaceToken {
                        span: ident.span(),
                        prefix: None,
                        suffix: None,
                    }));
                }

                TokenTree::Punct(p) if p.as_char() == '~' => match (tree.last_mut(), iter.peek()) {
                    (
                        Some(Self::Other(TokenTree::Ident(prefix))),
                        Some(TokenTree::Ident(ident)),
                    ) if ident.to_string() == replace => {
                        *tree.last_mut().unwrap() = Self::Replace(ReplaceToken {
                            span: prefix.span(),
                            prefix: Some(prefix.to_string()),
                            suffix: None,
                        });
                        iter.next();
                    }
                    (
                        Some(Self::Replace(ReplaceToken { suffix, .. })),
                        Some(TokenTree::Ident(sfx)),
                    ) if sfx.to_string() != replace => {
                        suffix.replace(sfx.to_string());
                        iter.next();
                    }
                    _ => tree.push(Self::Other(p.into())),
                },

                TokenTree::Group(g) => tree.push(Self::Group(GroupTree {
                    span: g.span(),
                    delimiter: g.delimiter(),
                    tree: Self::parse(g.stream(), replace),
                })),

                TokenTree::Punct(p) if p.as_char() == '#' => {
                    if let Some(TokenTree::Group(group)) = iter.peek()
                        && group.delimiter() == Delimiter::Parenthesis
                    {
                        let token_stream = group.stream();
                        let mut separator = Vec::new();

                        iter.next(); // group

                        for t in iter.by_ref() {
                            if let TokenTree::Punct(p) = &t
                                && p.as_char() == '*'
                            {
                                break;
                            }
                            separator.push(t);
                        }

                        tree.push(Self::Repeat(RepeatTree {
                            separator,
                            tree: Self::parse(token_stream, replace),
                        }));
                    } else {
                        tree.push(Self::Other(p.into()));
                    }
                }

                other => tree.push(Self::Other(other)),
            }
        }

        tree
    }

    fn to_token_stream(&self, start: usize, end: usize, i: usize) -> TokenStream {
        let mut token_stream = TokenStream::new();

        match self {
            Self::Group(x) => token_stream.extend([x.to_token_tree(start, end, i)]),
            Self::Repeat(x) => token_stream.extend(x.to_token_stream(start, end)),
            Self::Replace(x) => token_stream.extend([x.to_token_tree(i)]),
            Self::Other(x) => token_stream.extend([x.clone()]),
        }

        token_stream
    }
}

impl From<SeqIR> for TokenStream {
    fn from(val: SeqIR) -> Self {
        let SeqIR { start, end, tree } = val;
        let group = GroupTree {
            span: Span::call_site(),
            delimiter: Delimiter::None,
            tree,
        };

        if !group.naked_replace() && group.partial_repeat() {
            return group.to_token_tree(start, end, 0).into();
        }

        RepeatTree {
            separator: Vec::new(),
            tree: group.tree,
        }
        .to_token_stream(start, end)
    }
}

impl GroupTree {
    fn naked_replace(&self) -> bool {
        for token in &self.tree {
            match token {
                SeqTree::Group(t) if t.naked_replace() => return true,
                SeqTree::Replace(_) => return true,
                _ => (),
            }
        }
        false
    }
    fn partial_repeat(&self) -> bool {
        for token in &self.tree {
            match token {
                SeqTree::Group(t) if t.partial_repeat() => return true,
                SeqTree::Repeat(_) => return true,
                _ => (),
            }
        }
        false
    }

    fn to_token_tree(&self, start: usize, end: usize, i: usize) -> TokenTree {
        let mut token_stream = TokenStream::new();

        for token in &self.tree {
            token_stream.extend(token.to_token_stream(start, end, i));
        }

        let mut group = Group::new(self.delimiter, token_stream);
        group.set_span(self.span);
        TokenTree::Group(group)
    }
}

impl RepeatTree {
    fn to_token_stream(&self, start: usize, end: usize) -> TokenStream {
        let mut token_stream = TokenStream::new();

        if start >= end {
            return token_stream;
        }

        for token in &self.tree {
            token_stream.extend(token.to_token_stream(start, end, start));
        }

        if start + 1 == end {
            return token_stream;
        }

        for i in (start + 1)..end {
            token_stream.extend(self.separator.clone());
            for token in &self.tree {
                token_stream.extend(token.to_token_stream(start, end, i));
            }
        }

        token_stream
    }
}

impl ReplaceToken {
    fn to_token_tree(&self, val: usize) -> TokenTree {
        if self.prefix.is_none() && self.suffix.is_none() {
            return TokenTree::Literal(Literal::usize_unsuffixed(val));
        }

        TokenTree::Ident(Ident::new(
            &format!(
                "{}{val}{}",
                self.prefix.as_ref().map_or("", |v| v),
                self.suffix.as_ref().map_or("", |v| v),
            ),
            self.span,
        ))
    }
}

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    match SeqIR::try_from(input) {
        Ok(ir) => ir.into(),
        Err(ts) => ts,
    }
}

fn error(message: &str, span: Span) -> TokenStream {
    let (start, end) = (span.start(), span.end());

    TokenStream::from_iter([
        TokenTree::Punct({
            let mut x = Punct::new(':', Spacing::Joint);
            x.set_span(start);
            x
        }),
        TokenTree::Punct({
            let mut x = Punct::new(':', Spacing::Alone);
            x.set_span(start);
            x
        }),
        TokenTree::Ident(Ident::new("core", start)),
        TokenTree::Punct({
            let mut x = Punct::new(':', Spacing::Joint);
            x.set_span(start);
            x
        }),
        TokenTree::Punct({
            let mut x = Punct::new(':', Spacing::Alone);
            x.set_span(start);
            x
        }),
        TokenTree::Ident(Ident::new("compile_error", start)),
        TokenTree::Punct({
            let mut x = Punct::new('!', Spacing::Alone);
            x.set_span(start);
            x
        }),
        TokenTree::Group({
            let mut group = Group::new(Delimiter::Brace, {
                TokenStream::from_iter([TokenTree::Literal({
                    let mut msg = Literal::string(message);
                    msg.set_span(end);
                    msg
                })])
            });
            group.set_span(end);
            group
        }),
    ])
}
