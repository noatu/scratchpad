use std::iter::Peekable;

use proc_macro::{
    Delimiter, Group, Ident, Literal, Punct, Spacing, Span, TokenStream, TokenTree,
    token_stream::IntoIter,
};

/// An Intermediate Representation of the macro input
struct SeqIR {
    start: usize,
    end: usize,
    tree: Vec<SeqTree>, // like a TokenStream
}

/// Maps a `TokenTree` to a more friendly structure for replacing and repetition
enum SeqTree {
    Group {
        span: Span,
        delimiter: Delimiter,
        tree: Vec<SeqTree>,
    },
    Repeat {
        separator: Vec<TokenTree>,
        tree: Vec<SeqTree>,
    },
    Replace {
        span: Span,
        prefix: Option<String>,
        suffix: Option<String>,
    },
    Other(TokenTree),
}

impl TryFrom<TokenStream> for SeqIR {
    type Error = TokenStream;

    fn try_from(value: TokenStream) -> Result<Self, Self::Error> {
        let mut parser = Parser::new(value);

        let var = parser.parse(
            |tt| match tt {
                TokenTree::Ident(i) => Ok(i.to_string()),
                _ => Err(()),
            },
            "Expected an identifier!",
        )?;

        parser.parse(
            |tt| match tt {
                TokenTree::Ident(i) if i.to_string() == "in" => Ok(()),
                _ => Err(()),
            },
            "Expected the `in` keyword!",
        )?;

        let start = parser.parse(
            |tt| tt.to_string().parse::<usize>(),
            "Expected a usize (range start)!",
        )?;
        for _ in 0..2 {
            parser.parse(
                |tt| match tt {
                    TokenTree::Punct(p) if p.as_char() == '.' => Ok(()),
                    _ => Err(()),
                },
                "Expected `..`!",
            )?;
        }
        let end = parser
            .try_parse(|tt| match tt {
                TokenTree::Punct(p) if p.as_char() == '=' => Some(1),
                _ => None,
            })
            .unwrap_or(0)
            + parser.parse(
                |tt| tt.to_string().parse::<usize>(),
                "Expected a usize (range end)!",
            )?;

        let block = parser.parse(
            |tt| match tt {
                TokenTree::Group(g) => Ok(g.stream()),
                _ => Err(()),
            },
            "Expected a code block!",
        )?;

        parser.end()?;

        Ok(Self {
            start,
            end,
            // Parse only after checking for leftover tokens
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
                    tree.push(Self::Replace {
                        span: ident.span(),
                        prefix: None,
                        suffix: None,
                    });
                }

                // Handle the `prefix~Replace~suffix` syntax
                TokenTree::Punct(p) if p.as_char() == '~' => match (tree.last_mut(), iter.peek()) {
                    ( // prefix~Replace
                        Some(Self::Other(TokenTree::Ident(prefix))),
                        Some(TokenTree::Ident(ident)),
                    ) if ident.to_string() == replace => {
                        *tree.last_mut().unwrap() = Self::Replace {
                            span: prefix.span(),
                            prefix: Some(prefix.to_string()),
                            suffix: None,
                        };
                        iter.next();
                    }
                    // Replace~suffix
                    (Some(Self::Replace { suffix, .. }), Some(sfx))
                        // Cannot handle a Replace~Replace
                        if sfx.to_string() != replace =>
                    {
                        suffix.replace(sfx.to_string());
                        iter.next();
                    }
                    _ => tree.push(Self::Other(p.into())),
                },

                // Handle the repetition syntax
                TokenTree::Punct(p) if p.as_char() == '#' => {
                    if let Some(TokenTree::Group(group)) = iter.peek()
                        && group.delimiter() == Delimiter::Parenthesis
                    {
                        let token_stream = group.stream();
                        iter.next(); // skip the group

                        let separator = iter
                            .by_ref()
                            .take_while(
                                // fill the separator till '*' is found and consumed
                                |tt| !matches!(tt, TokenTree::Punct(p) if p.as_char() == '*'),
                            )
                            .collect();

                        tree.push(Self::Repeat {
                            separator,
                            tree: Self::parse(token_stream, replace),
                        });
                    } else {
                        tree.push(Self::Other(p.into()));
                    }
                }

                TokenTree::Group(g) => tree.push(Self::Group {
                    span: g.span(),
                    delimiter: g.delimiter(),
                    tree: Self::parse(g.stream(), replace),
                }),

                other => tree.push(Self::Other(other)),
            }
        }

        tree
    }

    fn to_token_stream(&self, start: usize, end: usize, i: usize) -> TokenStream {
        match self {
            Self::Group {
                span,
                delimiter,
                tree,
            } => {
                let mut group = Group::new(
                    *delimiter,
                    tree.iter()
                        .map(|st| st.to_token_stream(start, end, i))
                        .collect(),
                );
                group.set_span(*span);
                TokenTree::Group(group).into()
            }

            Self::Repeat { separator, tree } => {
                let mut token_stream = TokenStream::new();
                if start >= end {
                    return token_stream;
                }

                token_stream.extend(tree.iter().map(|st| st.to_token_stream(start, end, start)));
                if start + 1 == end {
                    return token_stream;
                }
                for i in (start + 1)..end {
                    token_stream.extend(separator.clone());
                    token_stream.extend(tree.iter().map(|st| st.to_token_stream(start, end, i)));
                }

                token_stream
            }

            Self::Replace {
                span,
                prefix,
                suffix,
            } => {
                // Ident cannot start with a number
                if prefix.is_none() {
                    return TokenTree::Literal(Literal::usize_unsuffixed(i)).into();
                }
                TokenTree::Ident(Ident::new(
                    &format!(
                        "{}{i}{}",
                        prefix.as_ref().map_or("", |v| v),
                        suffix.as_ref().map_or("", |v| v),
                    ),
                    *span,
                ))
                .into()
            }

            Self::Other(x) => x.clone().into(),
        }
    }

    /// Check if there is a Replace token that is not in a Repeat token
    fn bare_replace(&self) -> bool {
        match self {
            Self::Group { tree, .. } if tree.iter().any(Self::bare_replace) => true,
            Self::Replace { .. } => true,
            _ => false,
        }
    }
    /// Check if there is a nested Repeat token
    fn nested_repeat(&self) -> bool {
        match self {
            Self::Group { tree, .. } if tree.iter().any(Self::nested_repeat) => true,
            Self::Repeat { .. } => true,
            _ => false,
        }
    }
}

impl From<SeqIR> for TokenStream {
    fn from(val: SeqIR) -> Self {
        let SeqIR { start, end, tree } = val;

        // Without bare Replace tokens and with a nested Repeat the tree itself does not need repetition
        if !tree.iter().any(SeqTree::bare_replace) && tree.iter().any(SeqTree::nested_repeat) {
            return tree
                .iter()
                .map(|st| st.to_token_stream(start, end, 0))
                .collect();
        }

        // There is either a bare Replace token or no nested Repeats, meaning the tree itself is Repeat
        (start..end)
            .flat_map(|i| tree.iter().map(move |st| st.to_token_stream(start, end, i)))
            .collect()
    }
}

/// Repeats code inside the block. Example usage:
/// ```
/// use seq::seq;
///
/// seq!(N in 0..4 {
///     enum Enum~N { #( Variant~N~42, )* }
/// });
///
/// // hiiii
/// seq!(_ in 0..1 {
///     println!("hiiii");
/// });
/// // hiiiii
/// seq!(_ in 1..=5 {
///     println!(concat!("h" #(,"i")*));
/// });
/// // hiiiii
/// seq!(_ in 0..4 {
///     println!(concat!("h", #("i"),*, "i"));
/// });
/// // 1: hiiiii
/// // ...
/// // 5: hiiiii
/// seq!(N in 1..=5 {
///     println!(concat!(N, ": h"));
/// });
/// // hiiiii1
/// // ...
/// // hiiiii5
/// seq!(N in 1..=5 {
///     println!(concat!("h", #("i",)* N));
/// });
/// ```
#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    match SeqIR::try_from(input) {
        Ok(ir) => ir.into(),
        Err(ts) => ts,
    }
}

/// Interface to parse a `TokenStream` via closures. With error reporting.
struct Parser {
    prev_span: Span,
    iter: Peekable<IntoIter>,
}

impl Parser {
    fn new(token_stream: TokenStream) -> Self {
        Self {
            prev_span: Span::call_site(),
            iter: token_stream.into_iter().peekable(),
        }
    }

    /// Consume next `TokenTree`, drops `func` error value mapping it to `err`
    fn parse<F, T, E>(&mut self, func: F, err: &str) -> Result<T, TokenStream>
    where
        F: Fn(TokenTree) -> Result<T, E>,
    {
        let tt = self.iter.next().ok_or_else(|| error(err, self.prev_span))?;
        self.prev_span = tt.span();
        func(tt).map_err(|_| error(err, self.prev_span))
    }

    /// Advances the iter if `func` returns `Some`
    fn try_parse<F, T>(&mut self, func: F) -> Option<T>
    where
        F: Fn(&TokenTree) -> Option<T>,
    {
        let tt = self.iter.peek()?;
        if let Some(t) = func(tt) {
            self.prev_span = tt.span();
            self.iter.next();
            return Some(t);
        }
        None
    }

    /// Errors if there are leftover tokens
    fn end(self) -> Result<(), TokenStream> {
        let other = self
            .iter
            .map(|tt| error("Unexpected token!", tt.span()))
            .collect::<TokenStream>();
        if other.is_empty() { Ok(()) } else { Err(other) }
    }
}

/// Spanned `::core::compile_error! { message }`
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
