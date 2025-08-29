use proc_macro::TokenStream;
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::{ToTokens, format_ident, quote};
use syn::{
    Data, DeriveInput, Error, Expr, Field, Fields, GenericArgument, Ident, Lit, PathArguments,
    Result, Type,
    parse::{Parse, ParseStream},
    parse_macro_input,
    spanned::Spanned,
};

struct Builder {
    ident: Ident,
    origin_ident: Ident,
    fields: Vec<BuilderField>,
}

impl Parse for Builder {
    fn parse(input: ParseStream) -> Result<Self> {
        Self::parse(DeriveInput::parse(input)?)
    }
}

impl Builder {
    fn parse(input: DeriveInput) -> Result<Self> {
        let span = input.span();

        let Data::Struct(data_struct) = input.data else {
            return Err(Error::new(span, "can be used on a struct only!"));
        };
        let Fields::Named(fields_named) = data_struct.fields else {
            return Err(Error::new(span, "struct fields must be named!"));
        };

        let mut fields = Vec::new();

        for field in fields_named.named {
            fields.push(BuilderField::parse(field)?);
        }

        Ok(Self {
            ident: format_ident!("{}Builder", input.ident),
            origin_ident: input.ident,
            fields,
        })
    }
}

impl ToTokens for Builder {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let Self {
            ident,
            origin_ident,
            fields,
        } = self;

        let builder_body = fields
            .iter()
            .map(|BuilderField { ident, ty, kind }| match kind {
                FieldKind::Required | FieldKind::Option => {
                    quote!(#ident: ::std::option::Option<#ty>)
                }
                FieldKind::Vec(_) => quote!(#ident: ::std::vec::Vec<#ty>),
            });

        let builder_functions = fields
            .iter()
            .map(|BuilderField { ident, ty, kind }| match kind {
                FieldKind::Required | FieldKind::Option => quote! {
                   pub fn #ident (&mut self, #ident: #ty) -> &mut Self {
                       self.#ident = ::std::option::Option::Some(#ident);
                       self
                   }
                },
                FieldKind::Vec(each) => {
                    let mut q = quote! {
                    pub fn #ident (&mut self, #ident: ::std::vec::Vec<#ty>) -> &mut Self {
                        self.#ident = #ident;
                        self
                    }};

                    if each != ident {
                        q.extend(quote! {
                           pub fn #each (&mut self, #ident: #ty) -> &mut Self {
                               self.#ident.push(#ident);
                               self
                           }
                        });
                    }
                    q
                }
            });

        let constructor_fields = fields.iter().map(|BuilderField { ident, kind, .. }| {
            let err = format!("field '{ident}' is not set!");
            match kind {
                FieldKind::Option | FieldKind::Vec(_) => quote!(#ident: self.#ident.clone()),
                FieldKind::Required => {
                    quote!(#ident: self.#ident.clone().ok_or(#err)?)
                }
            }
        });

        tokens.extend(quote! {
            #[derive(::std::default::Default)]
            pub struct #ident {
               #(#builder_body,)*
            }

            impl #ident {
               #(#builder_functions)*

                pub fn build(&mut self) -> ::std::result::Result<#origin_ident, ::std::boxed::Box<dyn ::std::error::Error>> {
                    ::std::result::Result::Ok(#origin_ident {
                        #(#constructor_fields,)*
                    })
                }
            }

            impl #origin_ident {
                pub fn builder() -> #ident {
                    <#ident as ::std::default::Default>::default()
                 }
            }
        });
    }
}

struct BuilderField {
    ident: Ident,
    ty: Type,
    kind: FieldKind,
}

impl BuilderField {
    fn parse(field: Field) -> Result<Self> {
        let span = field.span();

        let mut result = Self {
            ident: field
                .ident
                .ok_or_else(|| Error::new(span, "Field must be named!"))?,
            ty: field.ty,
            kind: FieldKind::Required,
        };

        let mut each_attr = None;

        for attr in &field.attrs {
            if !attr.path().is_ident("builder") {
                continue;
            }

            if let Ok(Expr::Assign(expr)) = attr.parse_args::<Expr>()
                && let Expr::Path(p) = *expr.left
                && let Expr::Lit(lit) = *expr.right
                && let Lit::Str(lit_str) = lit.lit
            {
                if !p.path.is_ident("each") {
                    return Err(Error::new(p.span(), "expected `each`"));
                }

                let value = lit_str.value();
                if value.is_empty() {
                    return Err(Error::new(attr.span(), "function name can't be empty"));
                }
                if each_attr.replace(value).is_some() {
                    return Err(Error::new(attr.span(), "duplicate `each` attribute"));
                }
            } else {
                return Err(Error::new(
                    attr.span(),
                    "expected `builder(each = \"...\")`",
                ));
            }
        }

        if let Type::Path(type_path) = &result.ty
            && let Some(segment) = type_path.path.segments.last()
            && let PathArguments::AngleBracketed(gen_args) = &segment.arguments
            && let Some(GenericArgument::Type(gen_ty)) = gen_args.args.last()
        {
            if segment.ident == "Option" {
                result.ty = gen_ty.clone();
                result.kind = FieldKind::Option;
            } else if let Some(s) = each_attr
                && segment.ident == "Vec"
            {
                result.ty = gen_ty.clone();
                result.kind = FieldKind::Vec(Ident::new(&s, Span::call_site()));
            }
        }

        Ok(result)
    }
}

enum FieldKind {
    Required,   // T
    Option,     // Option<T>
    Vec(Ident), // Vec<T>
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let builder = parse_macro_input!(input as Builder);
    quote!(#builder).into()
}
