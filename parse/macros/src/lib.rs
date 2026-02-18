use num_traits::One;
use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use std::{ops::Add, str::FromStr};
use syn::{
    Data, DataEnum, Error, Expr, ExprClosure, ExprLit, Field, Fields, FieldsNamed, FieldsUnnamed,
    Ident, Lit, LitInt, Pat, PatLit, PatRange, Path, RangeLimits, Result, parse::Parse,
    parse_quote,
};

#[proc_macro_derive(Parsed, attributes(bitflags, map))]
pub fn parsed(input: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(input as syn::DeriveInput);

    let name = input.ident;

    let out = match input.data {
        Data::Struct(s) => {
            if cfg!(feature = "bitflags")
                && input
                    .attrs
                    .iter()
                    .any(|attr| attr.path().is_ident("bitflags"))
            {
                #[cfg(feature = "bitflags")]
                derive_bitflags(&name)
            } else {
                derive_struct_fields(s.fields, &name)
            }
        }
        Data::Enum(e) => {
            match input
                .attrs
                .iter()
                .find_map(|attr| {
                    attr.path()
                        .is_ident("repr")
                        .then(|| attr.parse_args_with(Ident::parse))
                })
                .transpose()
            {
                Ok(r) => derive_enum(e, &name, r),
                Err(e) => Err(e),
            }
        }
        Data::Union(u) => Err(Error::new_spanned(
            u.union_token,
            "Unions are not supported",
        )),
    };

    let body = match out {
        Ok(out) => proc_macro2::TokenStream::from(out),
        Err(err) => return err.to_compile_error().into(),
    };

    quote! {
        #[automatically_derived]
        impl ::leche_parse::Parsed for #name {
            fn parse(mut reader: impl ::std::io::Read) -> ::std::io::Result<Self> {
                #body
            }
        }
    }
    .into()
}

fn derive_struct_fields(fields: Fields, name: &Ident) -> Result<TokenStream> {
    match fields {
        Fields::Named(fields) => derive_named_fields(&fields, parse_quote!(#name)),
        Fields::Unnamed(fields) => derive_unnamed_fields(&fields, parse_quote!(#name)),
        Fields::Unit => Err(Error::new_spanned(name, "Unit structs are not supported")),
    }
}

fn derive_named_fields(fields: &FieldsNamed, parent: Path) -> Result<TokenStream> {
    let names = fields
        .named
        .iter()
        .map(|f| f.ident.as_ref().unwrap())
        .collect::<Vec<_>>();
    let types = fields.named.iter().map(|f| &f.ty).collect::<Vec<_>>();
    let maps = get_maps(&fields.named)?;

    Ok(quote! {
        Ok(#parent {#(
            #names: <#types as ::leche_parse::Parsed>::parse(&mut reader)#maps?
        ),*})
    }
    .into())
}

fn get_maps<'a>(
    fields: impl IntoIterator<Item = &'a Field>,
) -> Result<Vec<Option<proc_macro2::TokenStream>>> {
    fields
        .into_iter()
        .map(|f| {
            f.attrs.iter().find_map(|a| {
                a.path().is_ident("map").then(|| {
                    a.parse_args_with(ExprClosure::parse)
                        .map(|closure| quote!(.map(#closure)))
                })
            })
        })
        .map(Option::transpose)
        .collect::<Result<Vec<_>>>()
}

fn derive_unnamed_fields(fields: &FieldsUnnamed, parent: Path) -> Result<TokenStream> {
    let types = fields.unnamed.iter().map(|f| &f.ty).collect::<Vec<_>>();
    let maps = get_maps(&fields.unnamed)?;

    Ok(quote! {
        Ok(#parent(#(
            <#types as ::leche_parse::Parsed>::parse(&mut reader)#maps?,
        ),*))
    }
    .into())
}

#[cfg(feature = "bitflags")]
fn derive_bitflags(name: &Ident) -> Result<TokenStream> {
    Ok(quote! {
       ::bitflags::Flags::from_bits(
           <<Self as ::bitflags::Flags>::Bits>::parse(reader)?
       ).ok_or_else(|| {
           ::std::io::Error::new(
               ::std::io::ErrorKind::InvalidData,
               format!("invalid {}", ::std::stringify!(#name))
           )
       })
    }
    .into())
}

fn derive_enum(e: DataEnum, name: &Ident, mut repr: Option<Ident>) -> Result<TokenStream> {
    let variants = e
        .variants
        .iter()
        .map(|v| {
            let ident = &v.ident;
            let parent = parse_quote!(#name::#ident);
            match &v.fields {
                Fields::Named(fields) => derive_named_fields(fields, parent),
                Fields::Unnamed(fields) => derive_unnamed_fields(fields, parent),
                Fields::Unit => Ok(quote!(Ok(#parent)).into()),
            }
        })
        .map(|r| r.map(proc_macro2::TokenStream::from))
        .collect::<Result<Vec<_>>>()?;

    let tags = e.variants.iter().try_fold(
        Vec::with_capacity(e.variants.len()),
        |mut tags, v| -> Result<_> {
            let tag = if let Some((_, Expr::Lit(expr))) = &v.discriminant {
                Some(Ok(Pat::Lit(expr.clone())))
            } else {
                None
            }
            .or_else(|| match tags.last()? {
                Pat::Lit(PatLit {
                    lit: Lit::Int(lit), ..
                }) => Some(increment_litint_given_fallback(&mut repr, lit).map(|lit| {
                    Pat::Lit(PatLit {
                        lit: Lit::Int(lit),
                        attrs: Vec::new(),
                    })
                })),
                Pat::Range(range @ PatRange { limits, end, .. }) => {
                    let Some(Expr::Lit(ExprLit {
                        lit: Lit::Int(lit), ..
                    })) = end.as_deref()
                    else {
                        return Some(Err(Error::new_spanned(
                            range,
                            "a boundless tag range can only be used on the last variant",
                        )));
                    };

                    match limits {
                        RangeLimits::HalfOpen(_) => Some(Ok(Pat::Lit(PatLit {
                            lit: Lit::Int(lit.clone()),
                            attrs: Vec::new(),
                        }))),
                        RangeLimits::Closed(_) => {
                            Some(increment_litint_given_fallback(&mut repr, lit).map(|lit| {
                                Pat::Lit(PatLit {
                                    lit: Lit::Int(lit),
                                    attrs: Vec::new(),
                                })
                            }))
                        }
                    }
                }
                Pat::Lit(PatLit { lit, .. }) => Some(Err(Error::new_spanned(
                    lit,
                    "cannot auto-increment a non-int literal",
                ))),
                _ => unreachable!(),
            })
            .transpose()?
            .unwrap_or(Pat::Lit(PatLit {
                lit: Lit::Int(LitInt::new("0", Span::call_site())),
                attrs: Vec::new(),
            }));

            tags.push(tag);

            Ok(tags)
        },
    )?;

    let tag_type = repr.unwrap_or(parse_quote!(isize));

    Ok(quote! {
        match <#tag_type as ::leche_parse::Parsed>::parse(&mut reader)? {
            #(
                #tags => #variants,
            )*
            _ => Err(::std::io::Error::new(::std::io::ErrorKind::InvalidData, concat!("invalid tag for ", stringify!(#name))))
        }
    }
    .into())
}

fn increment_litint_given_fallback(repr: &mut Option<Ident>, lit: &LitInt) -> Result<LitInt> {
    let Some(fallback_type) = repr else {
        return increment_litint::<isize>(lit, repr);
    };

    macro_rules! match_fallback {
        ($($ty:ty),* $(,)?) => {
            match fallback_type.to_string().as_str() {
                $(
                    stringify!($ty) => Ok(increment_litint::<$ty>(lit, &mut None)?),
                )*
                _ => unreachable!(),
            }
        }
    }

    match_fallback!(
        u8, u16, u32, u64, u128, usize, i8, i16, i32, i64, i128, isize
    )
}

fn increment_litint<Fallback: FromStr + Add + One>(
    lit: &LitInt,
    repr: &mut Option<Ident>,
) -> Result<LitInt>
where
    Fallback::Err: std::fmt::Display,
    <Fallback as Add>::Output: ToString,
{
    macro_rules! match_suffix {
        ($($ty:ty),* $(,)?) => {
            match lit.suffix() {
                $(
                    ty @ stringify!($ty) => {
                        *repr = Some(Ident::new(ty, lit.span()));
                        Ok(LitInt::new(
                            &(lit.base10_parse::<$ty>().unwrap() + <$ty>::one()).to_string(),
                            Span::call_site(),
                        ))
                    },
                )*
                "" => Ok(LitInt::new(
                        &(lit.base10_parse::<Fallback>().unwrap() + Fallback::one()).to_string(),
                        Span::call_site(),
                )),
                _ => Err(Error::new_spanned(lit, "tag must be an integer"))
            }
        }
    }

    match_suffix!(
        u8, u16, u32, u64, u128, usize, i8, i16, i32, i64, i128, isize
    )
}
